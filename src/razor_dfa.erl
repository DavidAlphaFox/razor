-module(razor_dfa).

-export([minimize/2]).

-export([test/0]).


minimize(DFA, Groups) ->
    {GroupList, _} =
        lists:mapfoldl(
          fun (States, N) ->
                  case lists:member(1, States) of
                      true ->
                          {[{S, 1} || S <- States], N};
                      false ->
                          {[{S, N} || S <- States], N+1}
                  end
          end,
          2,
          Groups),
    GroupOf = dict:from_list(lists:append(GroupList)),

    DFA1 =
        [ { S,
            razor_group_map:compact(
              [ {A, dict:fetch(To, GroupOf)}
                || {A, To} <- Moves])
          }
          || {S, Moves} <- DFA ],

    case minimize_step(dict:from_list(DFA1), Groups) of
        Groups ->
            {GroupOf,
             dict:to_list(
               dict:from_list(
                 [{dict:fetch(From, GroupOf), Moves}
                  || {From, Moves} <- DFA1]))};
        Groups1 ->
            minimize(DFA, Groups1)
    end.

minimize_step(DFA, Groups) ->
    lists:append(
      [ case States of
            [_] ->
                [States];
            _ ->
                [ lists:usort(S)
                  || {_, S}
                         <- dict:to_list(
                              lists:foldl(
                                fun (S, Acc) ->
                                        dict:append(dict:fetch(S, DFA), S, Acc)
                                end,
                                dict:new(),
                                States))
                ]
        end
        || States <- Groups ]).

test_minimize(DFA, Groups) ->
    {GroupOf, DFA1} = minimize(DFA, Groups),
    {dict:to_list(GroupOf), DFA1}.

test(minimize) ->
    {[{3,2},{2,2},{1,1},{4,3}],
     [{2,[{[{0,0}],3}]},{1,[{[{0,1}],2}]}]} =
        test_minimize(
           [{1,[{[{0,0}],2},
                {[{1,1}],3}]},
            {2,[{[{0,0}],4}]},
            {3,[{[{0,0}],4}]}],
           [[1,2,3],[4]]),
    ok.

test() ->
    test(minimize),
    ok.
