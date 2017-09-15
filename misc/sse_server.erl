-module(sse_server).

-export([start/0]).
-export([init/1, loop/1, handle_connection/2]).
-export([send_event/1]).


start() ->
    start(8000).

start(Port) ->
    spawn(?MODULE, init, [Port]).


init(Port) ->
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,
           {packet, http_bin},
           {active, false},
           {reuseaddr, true}]),
    ?MODULE:loop(Socket).


loop(Socket) ->
    case gen_tcp:accept(Socket, 100) of
        {ok, Conn} ->
            Ref = make_ref(),
            Pid = spawn(?MODULE, handle_connection, [Ref, Conn]),
            ok = gen_tcp:controlling_process(Conn, Pid),
            Pid ! Ref;
        {error, timeout} ->
            ok
    end,
    ?MODULE:loop(Socket).


recv_headers(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Conn)];
        {ok, http_eoh} ->
            []
    end.


handle_connection(Ref, Conn) ->
    receive
        Ref ->
            ok
    after 1000 ->
            throw(timeout)
    end,

    {ok, {http_request, Method, {abs_path, Path}, Version}} = gen_tcp:recv(Conn, 0),
    Headers = recv_headers(Conn),
    ok = inet:setopts(Conn, [{packet, raw}]),

    try
        handle_request(Conn, Method, Path, Version, Headers)
    after
        ok = gen_tcp:close(Conn)
    end.


send_response_header(Conn, Code, ContentType, Headers) ->
    ok =
        gen_tcp:send(
          Conn,
          [<<"HTTP/1.1 ">>, integer_to_list(Code), <<" ">>, httpd_util:reason_phrase(Code), <<"\r\n">>,
           <<"Connection: close\r\n">>,
           <<"Content-Type: ">>, ContentType, <<"\r\n">>,
           Headers,
           <<"\r\n">>]).


response(Conn, Code, ContentType, Headers, Body) ->
    ok =
        send_response_header(
          Conn,
          Code,
          ContentType,
          [<<"Content-Length: ">>, integer_to_list(iolist_size(Body)), <<"\r\n">>|Headers]),
    ok = gen_tcp:send(Conn, Body).

send_chunk(Conn, Data) ->
    ok =
        gen_tcp:send(
          Conn,
          [erlang:integer_to_list(iolist_size(Data), 16), <<"\r\n">>,
           Data, <<"\r\n">>]).


handle_request(Conn, 'GET', <<"/sse">>, _Version, _) ->
    ok =
        send_response_header(
          Conn,
          200,
          <<"text/event-stream">>,
          [<<"Transfer-Encoding: chunked\r\n">>]),
    ?MODULE:send_event(Conn),
    ok;
handle_request(Conn, 'GET', <<"/">>, _Version, _Headers) ->
    {ok, Bin} = file:read_file(<<"index.html">>),
    response(Conn, 200, <<"text/html">>, [], Bin);
handle_request(Conn, 'GET', <<"/sse.js">>, _Version, _Headers) ->
    {ok, Bin} = file:read_file(<<"sse.js">>),
    response(Conn, 200, <<"application/javascript">>, [], Bin).


send_event(Conn) ->
    receive
    after 2000 ->
            send_chunk(Conn,
                       <<"data: stay tuned.\r\n\r\n">>)
    end,
    ?MODULE:send_event(Conn).
