-module(razor_url_example).

-compile({parse_transform, razor_url_dispatch}).

-export([test/0]).

-include("razor_url.hrl").

-dispatch({root, "posts/", {dispatch, post}}).
-dispatch({post, "", {endpoint, index}}).
-dispatch({post, "{id:integer}", {endpoint, post}}).

test() ->
    [<<"/">>,<<"posts/">>] =
        url_reverse(index, #{}),
    [<<"/">>,<<"posts/">>,<<"10">>] =
        url_reverse(post, #{id => 10}),
    {index,#{},<<>>} =
        url_dispatch(<<"/posts/">>),
    {index,#{},<<"a">>} =
        url_dispatch(<<"/posts/?a">>),
    {post,#{id := 10},<<>>} =
        url_dispatch(<<"/posts/10">>),
    {post,#{id := 10},<<>>} =
        url_dispatch(<<"/posts/1%30">>),
    error =
        url_dispatch(<<"/posts/10/">>),
    error =
        url_dispatch(<<"/posts/1%3">>),
    ok.
