-module(razor_thompson_nfa).

-export([from_re/1]).

-export([test/0]).

from_re(RE) ->
    transform(RE).

transform(RE) ->
    Seqs = [append([transform_elem(E) || E <- Seq]) || Seq <- RE],
    States = [N || {N, _} <- Seqs],
    E = 2 + lists:sum(States) - 2 * length(States),
    {Trans, Next} =
        lists:mapfoldl(
          fun({N, Trans}, M) ->
                  %%  1  2        N-1 N
                  %%  +--+- - - - -+--+
                  %%  1 2+M      N-1+M E
                  %%               1  2
                  %%               +--+- - -
                  %%               1 N+M
                  {razor_nfa:renum(Trans, M, 1, N, E), M+N-2}
          end,
          0,
          Seqs),
    {Next+2, lists:append(Trans)}.

append(List) ->
    {Trans, Next} =
        lists:mapfoldl(
          fun ({N, E}, M) ->
                  {razor_nfa:renum(E, M, N), N+M-1}
          end,
          0,
          List),
    {Next+1, lists:append(Trans)}.


%% TODO rename Ex to Tx
transform_elem({repeat, 0, infinity, E}) ->
    {N1, E1} = transform_elem(E),
    E2 = razor_nfa:renum(E1, 0, N1, N1, 1),
    {N1, [{1, e, N1}|E2]};
transform_elem({repeat, X, infinity, E}) ->
    E1 = transform_elem(E),
    {N2, E2} = append(lists:duplicate(X-1, E1)),
    {N3, E3} = E1,
    E4 = razor_nfa:renum(E3, N2-1, N3),
    {N2+N3-1, lists:append([E2, E4, [{N2+N3-1, e, N2}]])};
transform_elem({repeat, X, X, E}) ->
    append(lists:duplicate(X, transform_elem(E)));
transform_elem({repeat, X, Y, E}) ->
    E1 = transform_elem(E),
    {N2, E2} = append(lists:duplicate(X, E1)),
    {N3, E3} = append(lists:duplicate(Y-X, E1)),
    %%  1        N2
    %%  +- - - - +
    %%           1        N3
    %%           +- - - - +
    %%           N2       N2+N3-1
    E4 = razor_nfa:renum(E3, N2-1, N3),
    {N2+N3-1, lists:append([E2, E4, [{N2, e, N2+N3-1}]])};
transform_elem({group, E}) ->
    transform(E);
transform_elem({set, Set}) ->
    {2, [{1, Set, 2}]};
transform_elem(C) when is_integer(C) ->
    {2, [{1, [{C,C}], 2}]}.


test() ->
    {2, [{1,[{0,0}],2}]} = transform_elem(0),
    {2, [{1,[{0,3}], 2}]} = transform_elem({set, [{0,3}]}),
    {4, [{1,0,2},{2,1,3},{3,2,4}]} = transform([[0,1,2]]),
    {5,[{1,0,2},{2,0,5},{1,1,3},{3,1,5},{1,2,4},{4,2,5}]} = transform([[0,0],[1,1],[2,2]]),
    {5,[{1,0,2},{2,1,3},{3,2,4},{4,3,5}]} = transform([[0, {group, [[1,2]]}, 3]]),
    {6,[{1,0,2},{2,1,3},{3,2,5},{2,3,4},{4,4,5},{5,5,6}]} = transform([[0, {group, [[1,2],[3,4]]}, 5]]),
    {2, []} = transform([]),
    {3, [{1,0,2},{2,0,3}]} = transform([[{repeat, 2, 2, 0}]]),
    {1, []} = transform([[{repeat, 0, 0, 0}]]),
    {4,[{1,0,2},{2,0,3},{3,0,4},{2,e,4}]} = transform([[{repeat, 1, 3, 0}]]),
    {4,[{1,0,2},{2,0,3},{3,0,4},{1,e,4}]} = transform([[{repeat, 0, 3, 0}]]),
    {2,[{1,e,2},{2,0,1}]} = transform([[{repeat, 0, infinity, 0}]]),
    {3,[{1,0,2},{2,0,3},{3,e,2}]} = transform([[{repeat, 2, infinity, 0}]]),
    ok.
