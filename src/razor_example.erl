-module(razor_example).
-compile(
   {parse_transform,
    razor_url_dispatch}).
-dispatch(
   {root, "",
    {endpoint, homepage}}).

-export([start/0]).
-export([url_dispatch/1,
         url_reverse/2]).
-export([handle_request/3,
         middlewares/0,
         not_found/0]).


start() ->
    start(8000).

start(Port) ->
    razor_http_server:start(
      Port, [], ?MODULE,
      <<"x-powered-by: "
        "PHP/7.1.8\r\n">>).


handle_request('GET',homepage,_) ->
    html_response(
      200, <<"It works!">>);
handle_request(_,_,_) ->
    html_response(
      405,
      <<"405 - Method Not "
        "Allowed">>).


middlewares() -> [].

not_found() ->
    html_response(
      404, <<"404 - Not Found">>).


html_response(Code, Body) ->
    { http_response,
      Code,
      <<"Content-Type: text/html; "
        "charset=utf-8\r\n">>,
      simple_page(Body)
    }.


simple_page(Bin) ->
    razor_html:format(
      {html, [],
       [{head, [], [{title, [], [Bin]}]},
        {body, [], [Bin]}
       ]}).
