-module(razor_nfa).

-export(
   [ from_re/1, from_re/2,
     to_dfa/1,
     renum/3, renum/5,
     to_dict/1]).

-export(
   [ test/0 ]).

from_re(RE) ->
    from_re(RE, thompson).

from_re(RE, thompson) ->
    razor_thompson_nfa:from_re(RE).


dict_fetch(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        error ->
            Default;
        {ok, Value} ->
            Value
    end.

closure(List) ->
    {Closure, _Froms, _Tos} =
        razor_utils:traverse(
          List,
          {sets:new(), dict:new(), dict:new()},
          fun (Value, {Visited, _, _}) ->
                  sets:is_element(Value, Visited)
          end,
          fun({From, To}, {Visited, Froms, Tos}) ->
                  {[ {F, To}
                     || F <- dict_fetch(From, Froms, []) ]
                   ++ [ {From, T}
                        || T <- dict_fetch(To, Tos, []) ],
                   {sets:add_element({From, To}, Visited),
                    dict:append(To, From, Froms),
                    dict:append(From, To, Tos)}}
          end),
    sets:to_list(Closure).


epsilon_closure(NFA) ->
    lists:foldl(
      fun ({From, To}, D) ->
              dict:append(From, To, D)
      end,
      dict:new(),
      closure([ {From, To} || {From, e, To} <- NFA ])).


to_dict(NFA) ->
    lists:foldl(
      fun({From, A, To}, D) ->
              dict:append(From, {A,To}, D)
      end,
      dict:new(),
      NFA).


eliminate_epsilon(NFA) ->
    eliminate_epsilon(NFA, epsilon_closure(NFA)).

eliminate_epsilon(NFA, Closure) ->
    Trans = [ T || T = {_,A,_} <- NFA, A =/= e ],

    Moves = to_dict(Trans),

    [ {From, A, To}
      || {From, States} <- dict:to_list(Closure),
         State <- States,
         {A, To} <- dict_fetch(State, Moves, [])
    ] ++ Trans.


to_dfa(NFA) ->
    Closure = epsilon_closure(NFA),

    Moves =
        dict:map(
          fun(_, V) ->
                  merge_moves([{A, ordsets:from_list([To])} || {A, To} <- V])
          end,
          to_dict(eliminate_epsilon(NFA, Closure))),

    DFA =
        razor_utils:traverse(
          [ordsets:from_list([1])],
          dict:new(),
          fun dict:is_key/2,
          fun (States, Result) ->
                  Moves1 =
                      merge_moves(
                        [ M
                          || S <- States,
                             M <- dict_fetch(S, Moves, []) ]),
                  {[ D || {_,D} <- Moves1],
                   dict:store(States, Moves1, Result)
                  }
          end
         ),

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
    {[ { N,
         lists:usort(
           [ S
             || State <- States,
                S <- [State|dict_fetch(State, Closure, [])]
           ])
       }
       || {States, N} <- DFANumberList ],
     [ {dict:fetch(From, DFANumbers), [{A, dict:fetch(To, DFANumbers)} || {A, To} <- M]}
       || {From, M} <- dict:to_list(DFA) ]}.


merge_moves(List) ->
    razor_group_map:compact(
      razor_group_map:from_list(List)).


renum(Trans, M, N) ->
    %%  1         N
    %%  +- - - - -+
    %% 1+M       N+M
    %%            1
    %%            +- - - -
    %%           N+M
    renum(Trans, M, 1+M, N, N+M).

renum(Trans, M, S, N, E) ->
    [{renum_state(From, M, S, N, E), Bits, renum_state(To, M, S, N, E) }
     || {From, Bits, To} <- Trans].

renum_state(1, _, S, _, _) ->
    S;
renum_state(N, _, _, N, E) ->
    E;
renum_state(X, M, _, _, _) ->
    X + M.


test(closure) ->
    [{1,2},{1,3},{1,4},{1,5},{2,3},{2,4},{2,5},{3,4},{3,5},{4,5}] =
        lists:usort(closure([{2,3},{3,4},{1,2},{4,5}])),

    [{2,2},{2,3},{2,4},{3,2},{3,3},{3,4},{4,2},{4,3},{4,4}] =
        lists:usort(closure([{2,3},{3,4},{4,2}])),
    ok;
test(eliminate_epsilon) ->
    [{2,0,2},{1,0,2}] = eliminate_epsilon([{1,0,2},{2,e,1}]),
    [{2,2,4},{1,2,4},{1,1,3},{1,0,2},{2,1,3},{3,2,4}] =
        eliminate_epsilon([{1,0,2},{2,1,3},{3,2,4},{1,e,2},{2,e,3}]),
    ok;
test(to_dfa) ->
    {[{2,[2]},{1,[1]}],[{2,[]},{1,[{[{0,2}],2}]}]} =
        to_dfa([{1,[{0,0}],2},{1,[{1,1}],2},{1,[{2,2}],2}]),
    {[{2,[2,3]},{3,[2]},{4,[3]},{1,[1]}],
     [{2,[]},
      {3,[]},
      {4,[]},
      {1,[{[{1,2}],2},{[{0,0}],3},{[{3,3}],4}]}]} =
        to_dfa([{1,[{0,2}],2},{1,[{1,3}],3}]),
    ok.


test() ->
    test(closure),
    test(eliminate_epsilon),
    test(to_dfa),
    ok.
