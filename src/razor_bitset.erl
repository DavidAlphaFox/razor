-module(razor_bitset).

-export(
   [ from_range_list/1,
     to_range_list/1,
     union/2,
     intersection/2,
     subtract/2 ]).

-export([test/0]).


from_range_list(L) ->
    lists:foldl(
      fun (X, A) ->
              A bor (1 bsl X)
      end,
      0,
      [I || {X,Y} <- L,
            I <- lists:seq(X,Y)]).


to_range_list(X) ->
    razor_range_list:from_list(to_range_list(X, 0)).

to_range_list(0, _) ->
    [];
to_range_list(X, N) ->
    case X band 1 of
        0 ->
            to_range_list(X bsr 1, N+1);
        1 ->
            [{N, N}|to_range_list(X bsr 1, N+1)]
    end.


union(X, Y) ->
    X bor Y.

intersection(X, Y) ->
    X band Y.

subtract(X, Y) ->
    X bxor (X band Y).


test(from_range_list) ->
    2#10110 = from_range_list([{1,2},{4,4}]),
    ok;
test(to_range_list) ->
    [{1,2},{4,4}] = to_range_list(2#10110),
    ok.


test() ->
    test(from_range_list),
    test(to_range_list),
    ok.
