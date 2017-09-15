-module(razor_tagged_nfa).

-export([to_tagged_dfa/3]).

-export([test/0]).


to_tagged_dfa(NFA, TaggedTrans, EndStates) ->
    NFA1 =
        dict:map(
          fun(_, V) ->
                  merge_moves([{A, ordsets:from_list([To])} || {A, To} <- V])
          end,
          razor_nfa:to_dict(NFA)),

    DFA =
      razor_utils:traverse(
        [ordsets:from_list([1])],
        dict:new(),
        fun dict:is_key/2,
        fun (States, Result) ->
                Moves =
                    razor_group_map:from_list(
                      [ {A, ordsets:from_list([{To, {From, Tags}}])}
                        || {S, Tags, From} <- tagged_closure(States, TaggedTrans),
                           {A, L} <- dict_fetch(S, NFA1, []),
                           To <- L]),

                Moves1 =
                    razor_group_map:compact(
                      [{A, normalize_tagged_states(To)} || {A, To} <- Moves]),

                {[[S || {S,_} <- To] || {_,To} <- Moves1], dict:store(States, Moves1, Result)}
        end),

    {DFANumberList, _} =
        lists:mapfoldl(
          fun ([1], N) ->
                  {{[1], 1}, N};
              (S, N) ->
                  {{S, N}, N+1}
          end,
          2,
          dict:fetch_keys(DFA)),

    DFANumbers = dict:from_list(DFANumberList),

    { [{N, length(L)} || {L,N} <- DFANumberList],
      [ {N,hd(L)}
        || {N,L} <-
               [ {N,
                  [{index_of(From, L), Tags, S}
                   || {S, Tags, From} <- tagged_closure(L, TaggedTrans),
                      lists:member(S, EndStates) ]
                 }
                 || {L, N} <- DFANumberList],
           case L of
               [_] ->
                   true;
               [] ->
                   false
           end
      ],

      [{dict:fetch(From, DFANumbers),
        [ {A,
           {dict:fetch([S || {S,_} <- To], DFANumbers),
            [{index_of(F,From), Tags} || {_, {F,Tags}} <- To]}
          }
          || {A, To} <- M]}
       || {From, M} <- dict:to_list(DFA)]}.


index_of(X, L) ->
    index_of(X, 1, L).

index_of(X, N, [X|_]) ->
    N;
index_of(X, N, [_|T]) ->
    index_of(X, N+1, T).


tagged_closure(States, TaggedTrans) ->
    sets:to_list(
      razor_utils:traverse(
        [{S, ordsets:new(), S} || S <- States],
        sets:new(),
        fun sets:is_element/2,
        fun ({State, Tags, From}, Result) ->
                {[ {To, ordsets:add_element(Tag, Tags), From}
                   || {Tag, To} <- dict_fetch(State, TaggedTrans, [])],
                 sets:add_element({State, Tags, From}, Result)
                }
        end)).


dict_fetch(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        error ->
            Default;
        {ok, Value} ->
            Value
    end.

normalize_tagged_states(List) ->
    lists:keysort(
      1,
      dict:to_list(
        dict:map(
          fun (_, [H|T]) ->
                  lists:foldl(
                    fun(X = {_, T1}, Acc = {_, T2}) ->
                            case {ordsets:is_subset(T1,T2), ordsets:is_subset(T2,T1)} of
                                {true, true} ->
                                    throw(inconsistent);
                                {true, _} ->
                                    Acc;
                                {_, true} ->
                                    X;
                                _ ->
                                    throw(inconsistent)
                            end
                    end,
                    H,
                    T)
          end,
          lists:foldl(
            fun({To, {From, Tags}}, D) ->
                    dict:append(To, {From, Tags}, D)
            end,
            dict:new(),
            List)))).


merge_moves(List) ->
    razor_group_map:compact(
      razor_group_map:from_list(List)).


test(tagged_closure) ->
    [{2,[1],1},{4,[2,3],1},{3,[2],1},{1,[],1}] =
        tagged_closure([1], dict:from_list([{1, [{1, 2}, {2, 3}]}, {3, [{3, 4}]}])),
    ok;
test(to_tagged_dfa) ->
    ok =
        to_tagged_dfa(
          [{1, [{$a, $a}], 1},
           {2, [{$a, $a}], 3},
           {3, [{$a, $a}], 3}
          ],
          dict:from_list([{1, [{1, 2}]}]),
          [3]),
    ok.

test() ->
    test(tagged_closure),
    test(to_tagged_dfa),
    ok.
