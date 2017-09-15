-module(http_server).
-export([start/0,start/1]).
-export([init/1,loop/1,handle/1]).

start() -> start(8000).
start(Port) ->
    spawn(?MODULE, init, [Port]).


init(Port) ->
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,{packet, http_bin},
           {active, false},{reuseaddr, true}
          ]),
    ?MODULE:loop(Socket).


loop(S) ->
    case gen_tcp:accept(S, 100) of
        {ok, C} ->
            spawn(?MODULE,handle,[C]);
        {error, timeout} -> ok
    end,
    ?MODULE:loop(S).


handle(C) ->
    {ok,{http_request,M,P,_}} =
        gen_tcp:recv(C, 0),
    H = recv_headers(C),
    ok = inet:setopts(
           C,[{packet,raw}]),
    handle_request(C,M,P,H).


recv_headers(C) ->
    case gen_tcp:recv(C, 0) of
        {ok,{http_header,_,F,_,V}}->
            [{F,V}|recv_headers(C)];
        {ok, http_eoh} ->
            []
    end.


handle_request(C,'GET',_,_) ->
    Body = <<"OK.">>,
    ok = gen_tcp:send(C,
[<<"HTTP/1.1 200 OK\r\nConnection: "
   "close\r\nContent-Type: text/pla"
   "in\r\nContent-Length: ">>,
 integer_to_list(iolist_size(Body)),
 <<"\r\n\r\n">>,Body]).
