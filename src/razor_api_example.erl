-module(razor_api_example).
-compile({parse_transform,
          razor_url_dispatch}).
-compile({parse_transform,
          razor_db}).
-export([start/0, url_dispatch/1,
         url_reverse/2, handle_request/3,
         middlewares/0, not_found/0]).


-pattern(
   { integer,
     "[0-9]+",
     {erlang, binary_to_integer},
     {erlang, integer_to_binary}
   }).

-dispatch({root, "", {endpoint, index}}).
-dispatch({root, "{id:integer}", {endpoint, item}}).

start() ->
    start(8000).

start(Port) ->
    razor_http_server:start(Port, [], ?MODULE, []).

connect_db() ->
    epgsql:connect(
      "127.0.0.1",
      "razor", "",
      [{database, "razor"}]
     ).

middlewares() -> [].

not_found() ->
    response(404, <<"Not Found">>).

response(Code, Body) ->
    { http_response,
      Code,
      <<"Content-Type: text/plain; "
        "charset=utf-8\r\n">>,
      Body
    }.


handle_request('GET', index, _) ->
    with_db(
      fun(DB) ->
              Index =
                  razor_db:select(
                    DB,
                    [ io_lib:format("~w~n", [ID])
                      || #{id := ID} <- from(razor_item)
                    ]),
              response(200, Index)
      end);
handle_request('GET', item, #{id := X}) ->
    with_db(
      fun(DB) ->
              case razor_db:select(
                     DB,
                     [ Data
                       || #{id := ID,
                            data := Data} <- from(razor_item),
                          ID == X])
              of
                  [Data] -> response(200, Data);
                  [] -> not_found()
              end
      end);
handle_request('PUT', item, #{id := ID}) ->
    with_db(
      fun(DB) ->
              {ok, Data} = razor_http_server:read_body(),
              {ok, _} =
                  epgsql:equery(
                    DB,
                    "INSERT INTO razor_item(id,data) VALUES ($1,$2) ON CONFLICT(id) DO UPDATE SET data=$2",
                    [ID, Data]),
              {ok, _, _} = epgsql:squery(DB, "COMMIT"),
              response(200, <<"OK">>)
      end);
handle_request(_,_,_) ->
    response(
      405,
      <<"Method Not Allowed">>).

with_db(Fun) ->
    {ok, DB} = connect_db(),
    try
        {ok, _, _}
            = epgsql:squery(DB, "BEGIN"),
        Fun(DB)
    after
        epgsql:close(DB)
    end.
