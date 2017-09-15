-module(razor_url_dispatch).

-export([parse_transform/2, format_error/1]).

-export([test/0]).


parse_transform(Forms, _) ->
    { Forms1,
      #{ dispatches := Dispatches,
         patterns := Patterns,
         duplicates := Duplicates}} =
        lists:mapfoldl(
          fun({attribute, _, file, {Name, _}}=Form, A) ->
                  {[Form], A#{file => Name}};
             ({attribute, Line, dispatch, {Name, Rule, Action}}, A = #{dispatches := Dis, file := File}) ->
                  {[], A#{dispatches := dict:append(Name, {{File,Line}, Rule, Action}, Dis)}};
             ({attribute, Line, pattern, {Name,Regex,Decode,Encode}}, A = #{duplicates := Dup, patterns := P, file := File}) ->
                  { [],
                    case maps:is_key(Name, P) of
                        true ->
                            A#{duplicates := dict:append(Name, {File,Line}, Dup)};
                        false ->
                            A#{patterns := P#{Name => {{File,Line}, Regex, Decode, Encode}}}
                    end
                  };
             (F,A) ->
                  {[F],A}
          end,
          #{ dispatches => dict:new(),
             patterns => #{},
             duplicates => dict:new()
           },
          Forms),

    Forms2 = lists:append(Forms1),

    Forms3 =
        case check_dispatches(Dispatches) of
            {error, Errors} ->
                Forms2 ++ Errors;
            {ok, Dispatches1} ->
                case check_patterns(Patterns, dict:to_list(Duplicates), Dispatches1) of
                    {error, Errors} ->
                        Forms2 ++ Errors;
                    {ok, Patterns1, Warnings} ->
                        case build_url_reverse(Dispatches1, Patterns1) of
                            {error, Errors} ->
                                Forms2 ++ Errors ++ Warnings;
                            {ok, Reverse} ->
                                case build_url_dispatch(Dispatches1, Patterns1) of
                                    {error, Errors} ->
                                        Forms2 ++ Errors ++ Warnings;
                                    {ok, Dispatch} ->
                                        Forms2 ++ [Reverse] ++ Dispatch ++ Warnings
                                end
                        end
                end
        end,

    io:format(
      "After ~w:~n~s~n",
      [?MODULE, erl_prettypr:format(erl_syntax:form_list(Forms3))]),

    Forms3.


format_error({dispatch, Type, Name, Path}) ->
    io_lib:format(
      "dispatch '~w' ~s.~s",
      [Name,
       case Type of
           loop ->
               "loop";
           not_found ->
               "not found"
       end,
       format_error_path(Path)
      ]
     );
format_error(invalid_rule) ->
    "invalid rule.";
format_error(invalid_action) ->
    "invalid action.";
format_error({pattern_not_found, P}) ->
    io_lib:format("pattern '~w' not found.", [P]);
format_error({duplicate_pattern, P}) ->
    io_lib:format("duplicate pattern '~w'.", [P]);
format_error(invalid_regex) ->
    "invalid regular expression.";
format_error({endpoint_conflict, Path}) ->
    io_lib:format("endpoint conflict.~s", [format_error_path(Path)]);
format_error({duplicate_keys, Path}) ->
    io_lib:format("duplicate keys.~s", [format_error_path(Path)]);
format_error({rule_conflict, Path}) ->
    io_lib:format("rule conflict.~s", [format_error_path(Path)]);
format_error(_) ->
    ok.

format_error_path(Path) ->
    [io_lib:format("~n~s:~w:   required from here.", [F,L]) || {F,L} <- Path].

check_dispatches(Dispatches) ->
    case collect_dispatches(Dispatches) of
        {error, {Type, [{Name, {File,Line}}|Path]}} ->
            {error,
             [{attribute, Line, file, {File, Line}},
              {error, {Line, ?MODULE, {dispatch, Type, Name, [{F,L} || {_,{F,L}} <- Path]}}}]};
        {ok, Dispatches1} ->
            case parse_rules(Dispatches1) of
                {error, Locations} ->
                    {error,
                     lists:append(
                       [ [{attribute, Line, file, {File, Line}},
                          {error, {Line, ?MODULE, invalid_rule}}]
                         || {File,Line} <- Locations ])};
                {ok, Dispatches2} ->
                    case normalize_actions(Dispatches2) of
                        {error, Locations} ->
                            {error,
                             lists:append(
                               [ [{attribute, Line, file, {File, Line}},
                                  {error, {Line, ?MODULE, invalid_action}}]
                                 || {File,Line} <- Locations ])};
                        {ok, Dispatches3} ->
                            {ok, Dispatches3}
                    end
            end
    end.


collect_dispatches(Dispatches) ->
    case catch collect_dispatches({root, none}, [], sets:new(), Dispatches) of
        {loop, _} = Error ->
            {error, Error};
        {not_found, _} = Error ->
            {error, Error};
        Visited ->
            {ok,
             [{K,V}
              || {K,V} <- dict:to_list(Dispatches),
                 sets:is_element(K,Visited)]}
    end.


collect_dispatches({Name, Loc}, Path, Visited, Dispatches) ->
    case sets:is_element(Name, Visited) of
        true ->
            Visited;
        false ->
            Path1 = [{Name, Loc}|Path],

            case lists:keymember(Name, 1, Path) of
                true ->
                    throw({loop, Path1});
                false ->
                    case dict:find(Name, Dispatches) of
                        error ->
                            throw({not_found, Path1});
                        {ok, Dispatch} ->
                            sets:add_element(
                              Name,
                              lists:foldl(
                                fun (X, V) ->
                                        collect_dispatches(X, Path1, V, Dispatches)
                                end,
                                Visited,
                                [ {N,L} || {L, _, {dispatch, N}} <- Dispatch ]))
                    end
            end
    end.


parse_rules(Dispatches) ->
    Dispatches1 =
        [{Name, [{L, razor_url_rule:parse(R), A} || {L,R,A} <- D ]}
         || {Name, D} <- Dispatches ],

    case [ L
           || {_, D} <- Dispatches1,
              {L, error, _} <- D ]
    of
        [] ->
            {ok,
             [{Name, [{L,R,A} || {L,{ok,R},A} <- D ]}
              || {Name, D} <- Dispatches1 ]
            };
        Errors ->
            {error, Errors}
    end.


normalize_actions(Dispatches) ->
    Dispatches1 =
        [{Name, [{L,R,normalize_action(A)} || {L,R,A} <- D ]}
         || {Name, D} <- Dispatches ],

    case [ L
           || {_, D} <- Dispatches1,
              {L, _, error} <- D ]
    of
        [] ->
            {ok, Dispatches1};
        Errors ->
            {error, Errors}
    end.

normalize_action({endpoint, E}) when is_atom(E) ->
    {endpoint, E, #{}};
normalize_action({endpoint, E, M}) when is_atom(E), is_map(M) ->
    {endpoint, E, M};
normalize_action({dispatch, D}) when is_atom(D) ->
    {dispatch, D, #{}};
normalize_action({dispatch, D, M}) when is_atom(D), is_map(M) ->
    {dispatch, D, M};
normalize_action(_) ->
    error.


check_patterns(Patterns, Duplicates, Dispatches) ->
    case collect_patterns(Patterns, Dispatches) of
        {error, Errors} ->
            {error,
             lists:append(
               [ [{attribute, Line, file, {File, Line}},
                  {error, {Line, ?MODULE, {pattern_not_found, Pattern}}}]
                 || {Pattern, Locations} <- Errors,
                    {File, Line} <- Locations])};
        {ok, Patterns1} ->
            case check_duplicates(Patterns1, Duplicates) of
                {[], Warnings} ->
                    Warnings1 =
                        lists:append(
                          [ [{attribute, Line, file, {File, Line}},
                             {warning, {Line, ?MODULE, {duplicate_pattern, Pattern}}}]
                            || {Pattern, Locations} <- Warnings,
                               {File, Line} <- Locations]),

                    case parse_regex(Patterns1) of
                        {error, Errors} ->
                            {error,
                             lists:append(
                               [ [{attribute, Line, file, {File, Line}},
                                  {error, {Line, ?MODULE, invalid_regex}}]
                                 || {File, Line} <- Errors]) ++ Warnings1};
                        {ok, Patterns2} ->
                            {ok, Patterns2, Warnings1}
                    end;
                {Errors, Warnings} ->
                    {error,
                     lists:append(
                       [ [{attribute, Line, file, {File, Line}},
                          {error, {Line, ?MODULE, {duplicate_pattern, Pattern}}}]
                         || {Pattern, Locations} <- Errors,
                            {File, Line} <- Locations] ++
                           [ [{attribute, Line, file, {File, Line}},
                              {warning, {Line, ?MODULE, {duplicate_pattern, Pattern}}}]
                             || {Pattern, Locations} <- Warnings,
                                {File, Line} <- Locations])}
            end
    end.


collect_patterns(Patterns, Dispatches) ->
    Used =
        lists:foldl(
          fun({P, L}, Acc) ->
                  dict:append(P, L, Acc)
          end,
          dict:new(),
          [ {P, L}
            || {_, D} <- Dispatches,
               {L, R, _} <- D,
               {_, P} <- R ]),

    case [ {P, Ls}
           || {P, Ls} <- dict:to_list(Used),
              not maps:is_key(P, Patterns) ]
    of
        [] ->
            {ok,
             maps:filter(
               fun(K, _) -> dict:is_key(K, Used) end,
               Patterns)};
        Missing ->
            {error, Missing}
    end.


check_duplicates(Patterns, Duplicates) ->
    lists:partition(fun({Name, _}) -> maps:is_key(Name, Patterns) end, Duplicates).


parse_regex(Patterns) ->
    Patterns1 =
        [ {Name,{L,razor_regex:parse(R),D,E}}
          || {Name,{L,R,D,E}} <- maps:to_list(Patterns) ],

    case [ L || {_,{L,error,_,_}} <- Patterns1 ] of
        [] ->
            {ok,
             maps:from_list(
               [ {Name,{L,R,D,E}}
                 || {Name,{L,{ok,R},D,E}} <- Patterns1 ])};
        Errors ->
            {error, Errors}
    end.


build_url_dispatch(Dispatches, Patterns) ->
    Dispatches1 =
        [{Name,
          [{L,
            [case S of
                 {_,_} ->
                     S;
                 _ ->
                     str2nfa(S)
             end
             || S <- R],
            A}
           || {L, R, A} <- Dispatch]}
         || {Name, Dispatch} <- Dispatches],

    Patterns1 =
        maps:map(
          fun(_, {L,R,D,E}) ->
                  {L,re2nfa(R),D,E}
          end,
          Patterns),

    {N, Trans} = str2nfa("/"),

    {_, NFA, _, TaggedTrans, Endpoints} =
        build_url_dispatch(root, #{}, #{}, [], N, N, Trans, 1, dict:new(), #{}, dict:from_list(Dispatches1), Patterns1),

    {States, _} =
        razor_nfa:to_dfa(
          [ {From, e, To}
            || {From, T} <- dict:to_list(TaggedTrans),
               {_, To} <- T] ++ NFA),

    case
        [ L
          || L <-
                 [ [S || S <- L, maps:is_key(S, Endpoints)]
                   || {_, L} <- States
                 ],
             case L of
                 [] -> false;
                 [_] -> false;
                 _ -> true
             end
        ]
    of
        [] ->
            {Tags, EndStates, DFA} = razor_tagged_nfa:to_tagged_dfa(NFA, TaggedTrans, maps:keys(Endpoints)),
            {ok,
             [format_url_dispatch(maps:to_list(Endpoints)),
              format_url_dfa(dict:from_list(Tags), EndStates, DFA)]};
        Errors ->
            {error,
             lists:append(
               [ [{attribute, Line, file, {File, Line}},
                  {error, {Line, ?MODULE, {rule_conflict, Path}}}]
                 || {[{File,Line}|Path], _, _, _} <-
                        [ maps:get(S, Endpoints)
                          || L <- Errors,
                             S <- L ]
               ]
              )
            }
    end.

format_set_tag(T, []) ->
    format_tag(T);
format_set_tag(T, L) ->
    {map, 1, format_tag(T),
     [{map_field_assoc, 1, {integer,1,N}, {var,1,'P'}}
      || N <- L]}.


format_url_dfa(NTags, EndStates, DFA) ->
    {function, 1, url_dfa, 4,
     [{clause, 1,
       [{var, 1, 'P'},
        {bin, 1, [{bin_element, 1, {var, 1, 'C'}, default, default}, {bin_element, 1, {var, 1, 'Bin'}, default, [binary]}]},
        {integer, 1, From},
        {tuple, 1,
         [format_tag(I, lists:keymember(I,1,Tags))
          || I <- lists:seq(1, dict:fetch(From, NTags))]
        }
       ],
       [[{op, 1, '=<', {integer, 1, X}, {var, 1, 'C'}},
         {op, 1, '=<', {var, 1, 'C'}, {integer, 1, Y}}
        ]
        || {X,Y} <- Ranges
       ],
       [{call, 1, {atom,1,url_dfa},
         [{op, 1, '+', {var, 1, 'P'}, {integer, 1, 1}},
          {var, 1, 'Bin'},
          {integer, 1, To},
          {tuple, 1, [ format_set_tag(T,L) || {T, L} <- Tags]}
         ]
        }
       ]
      }
      || {From,Moves} <- DFA,
         {Ranges, {To, Tags}} <- Moves ]
     ++ lists:append(
          [[{clause, 1,
             [{var,1,
               case L of
                   [] ->
                       '_';
                   _ ->
                       'P'
               end
              },{bin,1,[]},{integer,1,From},
              {tuple,1,
               [format_tag(I, I=:=T)
                || I <- lists:seq(1, dict:fetch(From, NTags))]
              }
             ],
             [],
             [{tuple,1,[{integer,1,To}, format_set_tag(T,L), {bin,1,[]}]}]},
            {clause, 1,
             [{var,1,
               case L of
                   [] ->
                       '_';
                   _ ->
                       'P'
               end},
              {bin,1,
               [{bin_element,1,{char,1,$?},default,default},
                {bin_element,1,{var,1,'Rest'},default,[binary]}
               ]},
              {integer,1,From},
              {tuple,1,
               [format_tag(I, I=:=T)
                || I <- lists:seq(1, dict:fetch(From, NTags))]
              }
             ],
             [],
             [{tuple,1,
               [{integer,1,To}, format_set_tag(T,L), {var,1,'Rest'}]}]}
           ]
           || {From, {T, L, To}} <- EndStates
          ])
     ++ [{clause, 1, [{var,1,'_'},{var,1,'_'},{var,1,'_'},{var,1,'_'}], [], [{atom,1,error}]}]
    }.


format_url_dispatch(Endpoints) ->
    {function, 1, url_dispatch, 1,
     [{clause, 1,
       [{var, 1, 'Bin'}],
       [],
       [{'case', 1,
         {call, 1, {atom, 1, url_dfa}, [{integer,1,0},{var, 1, 'Bin'},{integer,1,1}, {tuple,1,[{map,1,[]}]}]},
         [{clause, 1, [{atom, 1, error}],[],[{atom,1,error}]}|
          [
           {clause, 1,
            [{tuple, 1,
              [{integer,1,N},
               {map,1,
                [{map_field_exact, 1, {integer, 1, T}, format_tag(T)}
                 || T <-
                        lists:usort(
                          [T
                           || {_,{Begin,End,_}} <- maps:to_list(Args),
                              T <- [Begin,End]])
                ]
               },
               {var,1,'Rest'}]
             }],
            [],
            [{tuple, 1,
              [{atom,1,Name},
               {map, 1,
                [ {map_field_assoc, 1, {atom,1, K},
                   {call,1,{remote,1,{atom,1,M},{atom,1,F}},
                    [{call,1,{remote,1,{atom,1,razor_url},{atom,1,decode}},
                      [{call,1,{remote,1,{atom,1,binary},{atom,1,part}},
                       [{var,1,'Bin'}, format_tag(Begin), {op,1,'-',format_tag(End),format_tag(Begin)}]
                       }
                      ]
                     }
                    ]
                   }
                  }
                  || {K, {Begin, End, {M,F}}} <- maps:to_list(Args) ]
                ++ [ {map_field_assoc, 1, {atom, 1, K}, format_literal(V)}
                     || {K, V} <- maps:to_list(Map)
                   ]},
               {var, 1, 'Rest'}
              ]
             }
            ]
           }
           || {N, {_, Name, Args, Map}} <- Endpoints
          ]
         ]
        }
       ]
      }]
    }.


build_url_dispatch(Dispatch, Map, Args, Path, CurrentState, MaxState, NFA, Tags, TaggedTrans, Endpoints, Dispatches, Patterns) ->
    lists:foldl(
      fun ({Loc, Rule, {Type, Name, M}}, {MaxState1, NFA1, Tags1, TaggedTrans1, Endpoints1}) ->
              {Args3, CurrentState3, MaxState3, NFA3, Tags3, TaggedTrans3} =
                  lists:foldl(
                    fun({N, Trans}, {Args2, State, MaxState2, NFA2, Tags2, TaggedTrans2}) when is_integer(N) ->
                            EndState = MaxState2+N-1,
                            {Args2, EndState, EndState, razor_nfa:renum(Trans, MaxState2 - 1, State, N, EndState) ++ NFA2, Tags2, TaggedTrans2};
                       ({Name2, Pattern}, {Args2, State, MaxState2, NFA2, Tags2, TaggedTrans2}) ->
                            {_, {N, Trans}, D, _} = maps:get(Pattern, Patterns),
                            EndState = MaxState2 + N,

                            { Args2#{Name2 => {Tags2, Tags2+1, D}},
                              EndState+1,
                              EndState+1,
                              razor_nfa:renum(Trans, MaxState2, N) ++ NFA2,
                              Tags2+2,
                              dict:append(State, {Tags2, MaxState2+1}, dict:append(EndState, {Tags2+1, EndState+1}, TaggedTrans2))
                            }
                    end,
                    {Args, CurrentState, MaxState1, NFA1, Tags1, TaggedTrans1},
                    Rule),
              Path1 = [Loc|Path],
              Map1 = maps:merge(Map, M),

              case Type of
                  dispatch ->
                      build_url_dispatch(Name, Map1, Args3, Path1, CurrentState3, MaxState3, NFA3, Tags3, TaggedTrans3, Endpoints1, Dispatches, Patterns);
                  endpoint ->
                      {MaxState3, NFA3, Tags3, TaggedTrans3,
                       Endpoints1#{CurrentState3 => {Path1, Name, Args3, Map1}}
                      }
              end
      end,
      {MaxState, NFA, Tags, TaggedTrans, Endpoints},
      dict:fetch(Dispatch, Dispatches)).


str2nfa(S) ->
    re2nfa([S]).

re2nfa(RE) ->
    RE1 = razor_url_regex:encode(razor_utf8_regex:encode(RE)),
    {End, NFA} = razor_nfa:from_re(RE1),
    {States, DFA} = razor_nfa:to_dfa(NFA),

    {G1,G2} =
        lists:partition(
          fun({_,L}) -> not lists:member(End,L) end,
          States),

    Group1 = lists:usort([S || {S,_} <- G1]),
    Group2 = lists:usort([S || {S,_} <- G2]),

    {States1, DFA1} = razor_dfa:minimize(DFA, [Group1,Group2]),
    Size = length(DFA1),
    Group2 =
        lists:usort(
          [S || {S, Sz} <- dict:to_list(States1), Sz =:= Size]),

    {Size,
     [{From, Action, To}
      || {From, Moves} <- DFA1,
         {Action, To} <- Moves]
    }.


build_url_reverse(Dispatches, Patterns) ->
    case catch build_url_reverse(root, [], [], ordsets:new(), #{}, #{}, dict:from_list(Dispatches)) of
        {endpoint_conflict, [{F1,L1}|P1], [{F2,L2}|P2]} ->
            {error,
             [{attribute, L1, file, {F1, L1}},
              {error, {L1, ?MODULE, {endpoint_conflict, P1}}},
              {attribute, L2, file, {F2, L2}},
              {error, {L2, ?MODULE, {endpoint_conflict, P2}}}]
            };
        {duplicate_keys, [{File,Line}|Path]} ->
            {error,
             [{attribute, Line, file, {File, Line}},
              {error, {Line, ?MODULE, {duplicate_keys, Path}}}]
            };
        ReverseMap ->
            Reverses =
                dict:map(
                  fun(_, V) ->
                          lists:reverse(lists:keysort(1, V))
                  end,
                  lists:foldl(
                    fun({{Name, Keys}, {_, Rule, Map}}, D) ->
                            dict:append(Name, {ordsets:size(Keys), Rule, Map}, D)
                    end,
                    dict:new(),
                    maps:to_list(ReverseMap))),
            {ok,
             {function, 1, url_reverse, 2,
              [ format_reverse_rule(Name, Rule, Map, Patterns)
                || {Name, Rules} <- dict:to_list(Reverses),
                   {_, Rule, Map} <- Rules ]}
            }
    end.


build_url_reverse(Name, Path, Prefix, Keys, Map, Result, Dispatches) ->
    lists:foldl(
      fun({Loc, Rule, {Type, N, M}}, R) ->
              Path1 = [Loc|Path],
              Keys1 = add_keys(Keys, [K || {K,_} <- Rule] ++ maps:keys(M), Path1),
              Prefix1 = Prefix ++ Rule,
              Map1 = maps:merge(Map, M),

              case Type of
                  dispatch ->
                      build_url_reverse(N, Path1, Prefix1, Keys1, Map1, R, Dispatches);
                  endpoint ->
                      case maps:find({N, Keys1}, R) of
                          error ->
                              R#{{N, Keys1} => {Path1, Prefix1, Map1}};
                          {ok, {Path2, _, _}} ->
                              throw({endpoint_conflict, Path1, Path2})
                      end
              end
      end,
      Result,
      dict:fetch(Name, Dispatches)).


add_keys(Keys, NewKeys, Path) ->
    NewKeys1 = ordsets:from_list(NewKeys),
    case ((length(NewKeys) > ordsets:size(NewKeys1))
          or (ordsets:size(ordsets:intersection(Keys, NewKeys1)) > 0))
    of
        true ->
            throw({duplicate_keys, Path});
        false ->
            ordsets:union(Keys, NewKeys1)
    end.


format_reverse_rule(Name, Rule, Map, Patterns) ->
    {clause, 1,
     [{atom,1,Name},
      {map, 1,
       [ {map_field_exact, 1, {atom, 1, A}, format_var(A)} || {A,_} <- Rule ]
       ++ [ {map_field_exact, 1, {atom, 1, K}, format_literal(V)}
            || {K, V} <- maps:to_list(Map)
          ]
      }
     ],
     [],
     [erl_syntax:revert(erl_syntax:list([ format_segment(S, Patterns) || S <- ["/"|Rule] ]))]}.


format_tag(T) ->
    {var, 1, list_to_atom([$T|integer_to_list(T)])}.

format_tag(T, true) ->
    format_tag(T);
format_tag(_, false) ->
    {var, 1, '_'}.

format_var(A) ->
    {var, 1, list_to_atom([$V|atom_to_list(A)])}.

format_literal(A) when is_atom(A) ->
    {atom, 1, A};
format_literal(I) when is_integer(I) ->
    {integer, 1, I};
format_literal(F) when is_float(F) ->
    {float, 1, F};
format_literal([]) ->
    {nil, 1};
format_literal([H|T]) ->
    {cons, 1, format_literal(H), format_literal(T)};
format_literal(T) when is_tuple(T) ->
    {tuple, 1, [format_literal(E) || E <- tuple_to_list(T)]};
format_literal(Bin) when is_binary(Bin) ->
    {bin, 1, [{bin_element,1, {string,1, unicode:characters_to_list(Bin)}, default, default}]}.

format_segment({Name, Type}, Patterns) ->
    {_,_,_,{M,F}} = maps:get(Type, Patterns),
    {call,1,
     {remote,1,{atom,1, razor_url},{atom,1,encode}},
     [{call,1,{remote,1,{atom,1,M},{atom,1,F}}, [format_var(Name)]}]};
format_segment(S, _) ->
    {bin, 1, [{bin_element,1, {string,1, S}, default, default}]}.


test(collect_dispatches) ->
    {error,{loop,[{root,{"x.erl",1}},{root,none}]}} =
        collect_dispatches(
          dict:from_list(
            [{root, [{{"x.erl", 1}, "", {dispatch, root}}]}]
           )),

    {error,{loop,[{root,{"x.erl",2}},{page,{"x.erl",1}},{root,none}]}} =
        collect_dispatches(
          dict:from_list(
            [{root, [{{"x.erl", 1}, "", {dispatch, page}}]},
             {page, [{{"x.erl", 2}, "", {dispatch, root}}]}]
           )),

    {ok,[{root,[{{"x.erl",1},[],{endpoint,index}}]}]} =
        collect_dispatches(
          dict:from_list(
            [{root, [{{"x.erl", 1}, "", {endpoint, index}}]},
             {page, [{{"x.erl", 2}, "", {dispatch, root}}]}]
           )),
    ok;
test(parse_rules) ->
    {error, [{"x.erl",1}]} =
        parse_rules([{root, [{{"x.erl", 1}, "{", {endpoint,index}}]}]),
    {ok,[{root,[{{"x.erl",1},[{id,integer}],{endpoint,index}}]}]} =
        parse_rules([{root, [{{"x.erl", 1}, "{id:integer}", {endpoint,index}}]}]),
    ok;
test(normalize_actions) ->
    {ok,[{root,[{{"x.erl",1},[],{endpoint,index,#{}}}]}]} =
        normalize_actions([{root, [{{"x.erl",1}, "", {endpoint, index}}]}]),
    {ok,[{root,[{{"x.erl",1},[],{endpoint,index,#{id := 1}}}]}]} =
        normalize_actions([{root, [{{"x.erl",1}, "", {endpoint, index, #{id => 1}}}]}]),
    {ok,[{root,[{{"x.erl",1},[],{dispatch,index,#{}}}]}]} =
        normalize_actions([{root, [{{"x.erl",1}, "", {dispatch, index}}]}]),
    {ok,[{root,[{{"x.erl",1},[],{dispatch,index,#{id := 1}}}]}]} =
        normalize_actions([{root, [{{"x.erl",1}, "", {dispatch, index, #{id => 1}}}]}]),
    {error,[{"x.erl",1}]} =
        normalize_actions([{root, [{{"x.erl",1}, "", endpoint}]}]),
    ok;
test(collect_patterns) ->
    {error,[{integer,[{"x.erl",1}]}]} =
        collect_patterns(#{}, [{root, [{{"x.erl",1}, [{id, integer}], {endpoint, index}}]}]),
    {ok, #{integer := {{"x.erl",1},[],decode,encode}}} =
        collect_patterns(
          #{integer => {{"x.erl",1}, "", decode, encode},
            int => {{"x.erl",1}, "", decode, encode}},
          [{root, [{{"x.erl",1}, [{id, integer}], {endpoint, index}}]}]),
    ok;
test(check_duplicates) ->
    {[{integer,[{"x.erl",2},{"x.erl",3}]}],[]} =
        check_duplicates(
          #{integer => {{"x.erl",1}, "", decode, encode}},
          [{integer, [{"x.erl",2}, {"x.erl",3}]}]),
    {[], [{integer,[{"x.erl",2},{"x.erl",3}]}]} =
        check_duplicates(
          #{},
          [{integer, [{"x.erl",2}, {"x.erl",3}]}]),
    ok;
test(parse_regex) ->
    {ok,#{integer :=
              {{"x.erl",1},
               [[{repeat,1,infinity,{set,[{48,57}]}}]],
               decode,encode}}} =
        parse_regex(#{integer => {{"x.erl",1}, "[0-9]+", decode, encode}}),
    {error,[{"x.erl",1}]} =
        parse_regex(#{integer => {{"x.erl",1}, "[", decode, encode}}),
    ok;
test(re2nfa) ->
    {2,[{1,[{47,47}],2}]}
        = str2nfa("/"),
    {ok, RE} = razor_regex:parse("[0-9]+"),
    ok = re2nfa(RE),
    ok.

test() ->
    test(collect_dispatches),
    test(parse_rules),
    test(normalize_actions),
    test(collect_patterns),
    test(check_duplicates),
    test(parse_regex),
    test(re2nfa),
    ok.
