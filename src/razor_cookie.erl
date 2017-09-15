-module(razor_cookie).

-export([middleware/4]).


decode(Bin) ->
    Parts = binary:split(Bin, [<<"; ">>], [global]),
    [ case binary:split(P, [<<"=">>], []) of
          [K,V] ->
              {K,V}
      end
      || P <- Parts ].


middleware(Method, Endpoint, Args, {M,F,A}) ->
    Cookies =
        case proplists:lookup('Cookie', get(headers)) of
            none ->
                [];
            {'Cookie', Cookie} ->
                decode(Cookie)
        end,

    put(cookies, Cookies),
    apply(M,F,[Method, Endpoint, Args|A]).
