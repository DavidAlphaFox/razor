-module(razor_range_list).

-export(
   [ new/0,
     is_empty/1,
     from_list/1,
     union/2,
     intersection/2,
     subtract/2]).

-export([test/0]).


new() ->
    [].

is_empty([]) ->
    true;
is_empty(_) ->
    false.


sort(S) ->
    lists:usort([{X,Y} || {X,Y} <- S, X =< Y]).

from_list(S) ->
    from_sorted_list(sort(S)).

from_sorted_list([]) ->
    [];
from_sorted_list([E]) ->
    [E];
from_sorted_list([{X1,Y1}, {X2,Y2}|T]) 
  when X2 =< Y1 + 1 ->
    from_sorted_list([{X1, Y2}|T]);
from_sorted_list([H|T]) ->
    [H|from_sorted_list(T)].


union(X, Y) ->
    from_list(X ++ Y).


intersection([], _) ->
    [];
intersection(_, []) ->
    [];
intersection([{_,Y1}|T1], L2=[{X2,_}|_])
  when Y1 < X2 ->
    intersection(T1, L2);
intersection(L1=[{X1,_}|_], [{_,Y2}|T2])
  when Y2 < X1 ->
    intersection(L1, T2);
intersection([{X1,Y}|T1], [{X2,Y}|T2]) ->
    [{max(X1,X2),Y}|intersection(T1,T2)];
intersection([{X1,Y1}|T1], [{X2,Y2}|T2])
  when Y1 < Y2 ->
    [{max(X1,X2),Y1}|intersection(T1,[{Y1+1,Y2}|T2])];
intersection([{X1,Y1}|T1], [{X2,Y2}|T2])
  when Y1 > Y2 ->
    [{max(X1,X2),Y2}|intersection([{Y2+1,Y1}|T1],T2)].


subtract([], _) ->
    [];
subtract(X, []) ->
    X;
subtract([{_,Y1}|T1], L2=[{X2,_}|_])
  when Y1 < X2 ->
    subtract(T1, L2);
subtract(L1=[{X1,_}|_], [{_,Y2}|T2])
  when X1 > Y2 ->
    subtract(L1, T2);
subtract([{X1,Y}|T1], [{X2,Y}|T2])
  when X1 < X2 ->
    [{X1,X2-1}|subtract(T1, T2)];
subtract([{_,Y}|T1], [{_,Y}|T2]) ->
    subtract(T1, T2);
subtract([{X1,Y1}|T1], [{X2,Y2}|T2])
  when X1 < X2, Y1 < Y2 ->
    [{X1,X2-1}|subtract(T1, [{Y1+1,Y2}|T2])];
subtract([{_,Y1}|T1], [{_,Y2}|T2])
  when Y1 < Y2 ->
    subtract(T1, [{Y1+1,Y2}|T2]);
subtract([{X1,Y1}|T1], [{X2,Y2}|T2])
  when X1 < X2, Y1 > Y2 ->
    [{X1,X2-1}|subtract([{Y2+1,Y1}|T1], T2)];
subtract([{_,Y1}|T1], [{_,Y2}|T2])
  when Y1 > Y2 ->
    subtract([{Y2+1,Y1}|T1], T2).


test(from_list) ->
    [{1,3}] = from_list([{1,1},{2,2},{3,3}]),
    [{1,5},{7,10}] = from_list([{1,4},{2,3},{3,5},{7,10}]),
    ok;
test(union) ->
    [{1,2}] = union([{1,1}],[{2,2}]),
    ok;
test(intersection) ->
    [{5,10},{12,20}] = intersection([{1,10},{12,30}], [{5,20}]),
    [{5,10},{12,30}] = intersection([{1,10},{12,30}], [{5,30}]),
    ok;
test(subtract) ->
    [{1,4},{21,30}] = subtract([{1,10},{12,30}],[{5,20}]),
    [{1,4}] = subtract([{1,10},{12,30}], [{5,30}]),
    ok.


test() ->
    test(from_list),
    test(union),
    test(intersection),
    test(subtract),
    ok.
