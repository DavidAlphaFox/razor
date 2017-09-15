-module(razor_http_server).

-export([start/4]).

-export([init/4, loop/3, start_connection/4]).

-export([handle_request/5, read_body/0, read_form/0]).


start(Port, Options, Module, ExtraHeaders) ->
    spawn(?MODULE, init, [Port, Options, Module, ExtraHeaders]).


init(Port, Options, Module, ExtraHeaders) ->
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,
           {packet, http_bin},
           {active, false},
           {reuseaddr, true}|Options]),
    ?MODULE:loop(Socket, Module, ExtraHeaders).


loop(Socket, Module, ExtraHeaders) ->
    case gen_tcp:accept(Socket, 200) of
        {ok, Conn} ->
            Ref = make_ref(),
            Pid = spawn(?MODULE, start_connection, [Ref, Conn, Module, ExtraHeaders]),
            ok = gen_tcp:controlling_process(Conn, Pid),
            Pid ! Ref;
        {error, timeout} ->
            ok
    end,
    ?MODULE:loop(Socket, Module, ExtraHeaders).


recv_headers(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Conn)];
        {ok, http_eoh} ->
            []
    end.


start_connection(Ref, Conn, Module, ExtraHeaders) ->
    receive
        Ref ->
            try
                handle_connection(Conn, Module, ExtraHeaders)
            after
                ok = gen_tcp:close(Conn)
            end
    after 1000 ->
            throw(timeout)
    end.


handle_connection(Conn, Module, ExtraHeaders) ->
    {ok, {http_request, Method, {abs_path, Path}, Version}} =
        gen_tcp:recv(Conn, 0),
    Headers = recv_headers(Conn),
    ok = inet:setopts(Conn, [{packet, raw}]),
    {http_response, Code, Headers1, Body} =
        handle_http_request(Conn, Method, Path, Version, Headers, Module),

    Response =
        [ <<"HTTP/1.1 ">>, integer_to_list(Code), <<" ">>, httpd_util:reason_phrase(Code), <<"\r\n">>,
          <<"Connection: close\r\n">>,
          <<"Content-Length: ">>, integer_to_list(iolist_size(Body)), <<"\r\n">>,
          ExtraHeaders,
          Headers1,
          <<"\r\n">>,
          Body],
    gen_tcp:send(Conn, Response).


handle_http_request(Conn, Method, Path, _Version, Headers, Module) ->
    case Module:url_dispatch(Path) of
        error ->
            Module:not_found();
        {Endpoint, Args, Query} ->
            put(connection, Conn),
            put(headers, Headers),
            put(query, razor_query:decode(Query)),
            ?MODULE:handle_request(
               Method, Endpoint, Args,
               Module, Module:middlewares())
    end.


handle_request(Method, Endpoint, Args, Module, []) ->
    Module:handle_request(Method, Endpoint, Args);
handle_request(Method, Endpoint, Args, Module, [{M,F,A}|Rest]) ->
    apply(M, F, [Method, Endpoint, Args, {?MODULE, handle_request, [Module, Rest]}|A]).


read_body() ->
    case proplists:lookup('Content-Length', get(headers)) of
        error ->
            {error, length_required};
        {'Content-Length', Length} ->
            gen_tcp:recv(get(connection), binary_to_integer(Length))
    end.


read_form() ->
    case get(form) of
        undefined ->
            case read_body() of
                {ok, Bin} ->
                    Form = razor_query:decode(Bin),
                    put(form, Form),
                    {ok, Form};
                Error ->
                    Error
            end;
        Form ->
            {ok, Form}
    end.
