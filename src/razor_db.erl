-module(razor_db).

-export([query/3]).

-export([parse_transform/2, format_error/1]).


query(Conn, SQL, Params) ->
    {ok, _, Rows} = epgsql:equery(Conn, SQL, Params),
    Rows.


parse_transform(Forms, _Options) ->
    Forms1 = lists:append([ form(F) || F <- Forms ]),

    io:format(
      "After ~w:~n~s~n",
      [?MODULE, erl_prettypr:format(erl_syntax:form_list(Forms1))]),

    Forms1.


format_error(illegal_cte) ->
    "illegal common table expression.";
format_error(output_name_conflict) ->
    "queries in common table expression do not have same output column names.";
format_error(not_lc) ->
    "query expression is not a list comprehension.";
format_error(illegal_output_expression) ->
    "illegal output expression.";
format_error({unused, V}) ->
    io_lib:format("variable ~w is unused.", [V]);
format_error({output_conflict, V}) ->
    io_lib:format("output column named '~w' already defined.", [V]);
format_error(illegal_select_expression) ->
    "illegal select expression.";
format_error(aggregate_in_join) ->
    "aggregate function not allowed in join expression.";
format_error(illegal_generate) ->
    "illegal generator in query expression.";
format_error(b_generate) ->
    "bitstring generator not allowed in query expression.";
format_error(illegal_order_expression) ->
    "illegal order expression.";
format_error(integer_not_allowed) ->
    "integer not allowed.";
format_error(illegal_expression) ->
    "illegal query expression.";
format_error(illegal_pattern) ->
    "illegal pattern.";
format_error({bound, V}) ->
    io_lib:format("variable ~w is bound.", [V]);
format_error({once, X}) ->
    io_lib:format("~w is allowed only once.", [X]);
format_error({unbound, V}) ->
    io_lib:format("variable ~w is unbound.", [V]);
format_error(no_table) ->
    "no table used in query expression.";
format_error(first_table_join) ->
    "first table must not be a join.";
format_error(unused_param) ->
    "param is unused".


form({function, _, _, _, _}=Form) ->
    Tree = erl_syntax_lib:annotate_bindings(Form, ordsets:new()),
    case catch erl_syntax_lib:mapfold(fun transform/2, [], Tree) of
        {error, Line, Error} ->
            [{error, {Line, ?MODULE, Error}}];
        {Tree1, Warnings} ->
            [erl_syntax:revert(Tree1) |
             [ {warning, {Line, ?MODULE, W}}
               || {Line, W} <- Warnings ]]
    end;
form(Form) ->
    [Form].


transform(
  {tree, application, {attr, Line, _, _},
   {application,
    {tree, module_qualifier, {attr, LineR, _, _},
     {module_qualifier,
      {wrapper, atom, _, {atom, LineM, ?MODULE}},
      {wrapper, atom, _, {atom, LineF, F}}
     }
    },
    Args
   }
  } = Tree, Warnings) ->
    case {F, Args} of
        {select, [C, LC]} ->
            {LC1, Meta} = visit_query(LC, new_meta()),
            Call = {call, Line, {remote, LineR, {atom, LineM, ?MODULE}, {atom, LineF, query}}},
            build_query(Line, Call, C, [], LC1, Meta, Warnings);
        {select, [C, M, LC]} ->
            {tree, map_expr,
             {attr, _, Vars, _},
             _
            } = M,
            Env = proplists:get_value(env, Vars),

            {CTEs, _, Meta} =
                visit_ctes(
                  erl_syntax:revert(M),
                  (new_meta())#{env => Env}),
            {LC1, Meta1} = visit_query(LC, Meta),
            Call = {call, Line, {remote, LineR, {atom, LineM, ?MODULE}, {atom, LineF, query}}},
            build_query(Line, Call, C, CTEs, LC1, Meta1, Warnings);
        _ ->
            {Tree, Warnings}
    end;
transform(Tree, Warnings) ->
    {Tree, Warnings}.

new_meta() ->
    #{
      next_table => 1,
      params => #{},
      values => {},
      stack => [],
      warnings => []
     }.


visit_ctes({map, _, Fields}, Meta) ->
    {Fields1, Meta1} =
        lists:mapfoldl(
          fun visit_cte/2,
          Meta,
          Fields),
    {Fields2, A} =
        lists:mapfoldl(
          fun({F, A1}, A2) ->
                  {F, merge_aggregates(A1, A2)}
          end,
          {ordsets:new(), ordsets:new()},
          Fields1),
    {Fields2, A, Meta1};
visit_ctes({_, _, {attr, Line, _, _}, _}, _) ->
    throw({error, Line, illegal_cte}).


visit_cte({map_field_assoc, Line, {atom, _, Key}, E}, Meta) ->
    {Query, A, Meta1} = visit_cte_query(E, Meta),

    Fields =
        [ ordsets:from_list(maps:keys(Outputs))
          || #{outputs := Outputs} <- Query],

    Union =
        lists:foldl(
          fun ordsets:union/2,
          ordsets:new(),
          Fields),

    Intersection =
        lists:foldl(
          fun ordsets:intersection/2,
          Union,
          Fields),

    case ordsets:size(ordsets:subtract(Union, Intersection)) of
        0 ->
            {{{Key, Intersection, Query}, A}, Meta1};
        _ ->
            throw({error, Line, output_name_conflict})
    end;
visit_cte(E, _) ->
    throw({error, element(2,E), illegal_cte}).


visit_cte_query({nil, _}, Meta) ->
    {[], {ordsets:new(), ordsets:new()}, Meta};
visit_cte_query({cons, _, H, T}, Meta) ->
    {H1, A1, Meta1} = visit_subquery(H, Meta),
    {T1, A2, Meta2} = visit_cte_query(T, Meta1),
    {[H1|T1], merge_aggregates(A1, A2), Meta2};
visit_cte_query(E, _) ->
    throw({error, element(2,E), not_proper_list}).


visit_query(
  {tree, list_comp,
   {attr, Line, InputVars, _},
   {list_comp, {_, _, {attr, _, OutputVars, _}, _}, _}
  } = Comp, Meta) ->
    {lc, _, Output, Qs} = erl_syntax:revert(Comp),
    Env = proplists:get_value(env, InputVars),
    Free = proplists:get_value(free, OutputVars),

    Meta1 = visit_qualifiers(Qs, Line, Meta#{env => Env}),
    #{ stack := [Query = #{vars := Vars, used := Used}] } = Meta1,

    Outputs =
        [ {V, X}
          || {V, X} <- maps:to_list(Vars),
             ordsets:is_element(V, Free) or not ordsets:is_element(V, Used)
        ],

    LC1 =
        {lc, Line,
         Output,
         [ {generate, Line,
            case Outputs of
                [] ->
                    {var, Line, '_'};
                _ ->
                    {tuple, Line,
                     [ {var, Line, V}
                       || {V, _} <- Outputs
                     ]
                    }
            end
           }
         ]
        },

    {LC1, Meta1#{stack := [Query#{outputs => [ X || {_, X} <- Outputs ]}]}};
visit_query({_, _, {attr, Line, _, _}, _}, _) ->
    throw({error, Line, not_lc}).

visit_subquery(none, LC, Meta) ->
    {Query, A, Meta1} = visit_subquery(LC, Meta),
    {{subquery, [], Query}, A, Meta1};
visit_subquery(Map, LC, Meta) ->
    {CTEs, A1, Meta1} = visit_ctes(Map, Meta),
    {Query, A2, Meta2} = visit_subquery(LC, Meta1),
    {{subquery, CTEs, Query}, merge_aggregates(A1, A2), Meta2}.


visit_subquery({lc, Line, E, Qs}, Meta) ->
    Meta1 = visit_qualifiers(Qs, Line, Meta),
    #{stack := [H = #{aggregates := A}|T]} = Meta2 =
        visit_output(E, Meta1),
    {H, A, add_aggregates(A, Meta2#{stack := T})};
visit_subquery(E, _) ->
    throw({error, element(2,E), not_lc}).


visit_output({map, _, Fields}, Meta) ->
    visit_output_fields(Fields, #{}, Meta);
visit_output(E, _) ->
    throw({error, element(2,E), illegal_output_expression}).

visit_output_fields([], Outputs, Meta = #{warnings := Warnings, stack := [H = #{used := Used, vars := Vars, lines := Lines}|T]}) ->
    Unused = ordsets:to_list(ordsets:subtract(ordsets:from_list(maps:keys(Vars)), Used)),
    Meta#{stack := [H#{outputs => Outputs}|T],
          warnings := Warnings ++ [{maps:get(V, Lines), {unused, V}} || V <- Unused ]};
visit_output_fields([{map_field_assoc, _, {atom, Line, Key}, E}|T], Outputs, Meta) ->
    case maps:is_key(Key, Outputs) of
        true ->
            throw({error, Line, {output_conflict, Key}});
        false ->
            {E1, _, Meta1} = visit_expression(E, none, Meta),
            visit_output_fields(T, Outputs#{Key => E1}, Meta1)
    end;
visit_output_fields([H|_], _, _) ->
    throw({error, element(2,H), illegal_output_expression}).


visit_qualifiers(Qs, Line, Meta = #{stack := Stack}) ->
    Stack1 =
        [ #{ line => Line,
             tables => {},
             wheres => {},
             havings => {},
             vars => #{},
             lines => #{},
             used => ordsets:new(),
             aggregate => #{},
             aggregates => {ordsets:new(), ordsets:new()}
           }
          | Stack],

    lists:foldl(fun visit_qualifier/2, Meta#{stack := Stack1}, Qs).


visit_qualifier({generate, _, P, {call, _, {atom, _, from}, [S]}}, Meta) ->
    {N, Table, Meta1} = visit_select(S, Meta),
    Meta2 = add_table({N, {from, Table}}, Meta1),
    visit_pattern(P, N, Meta2);
visit_qualifier({generate, _, P, {call, _, {atom, _, join}, [S|Exprs]}}, Meta) ->
    {N, Table, Meta1} = visit_select(S, Meta),
    {Exprs1, Meta2} =
        lists:mapfoldl(
          fun (X, M) ->
                  {E, A, M1} = visit_expression(X, N, M),
                  case is_aggregate(A, M1) of
                      false ->
                          {E, M1};
                      true ->
                          throw({error, element(2,X), aggregate_in_join})
                  end
          end,
          Meta1,
          Exprs),
    Meta3 = add_table({N, {join, Table, Exprs1}}, Meta2),
    visit_pattern(P, N, Meta3);

visit_qualifier({generate, _, {var,_,V}=Var, {cons,_,E,{nil,_}}}, Meta) ->
    {E1, A, Meta1} = visit_expression(E, none, Meta),
    set_aggregate(V, A, bind_var(Var, E1, Meta1));
visit_qualifier({generate, Line, _, _},  _) ->
    throw({error, Line, illegal_generate});
visit_qualifier({b_generate, Line, _, _},  _) ->
    throw({error, Line, b_generate});

visit_qualifier({call, _, {atom, Line, group_by}, [_|_] = Exprs}, Meta) ->
    {Exprs1, Meta1} =
        lists:mapfoldl(
          fun visit_column_expression/2,
          Meta,
          Exprs),
    set_key(group_by, Line, Exprs1, Meta1);

visit_qualifier({call, _, {atom, Line, order_by}, [_|_] = Exprs}, Meta) ->
    {Exprs1, Meta1} =
        lists:mapfoldl(
          fun visit_order_expression/2,
          Meta,
          Exprs),
    set_key(order_by, Line, Exprs1, Meta1);
visit_qualifier({call, _, {atom, Line, distinct}, Exprs}, Meta) ->
    {Exprs1, Meta1} =
        lists:mapfoldl(
          fun visit_column_expression/2,
          Meta,
          Exprs),
    set_key(distinct, Line, Exprs1, Meta1);
visit_qualifier({call, _, {atom, Line, A}, [E]}, Meta)
  when A =:= offset; A =:= limit ->
    {E1, _, Meta1} = visit_expression(E, none, Meta),
    set_key(A, Line, E1, Meta1);

visit_qualifier(Q, Meta) ->
    {Expr, A, Meta1} = visit_expression(Q, none, Meta),
    #{stack := [H|T]} = Meta1,

    Key =
        case is_aggregate(A, Meta1) of
            false ->
                wheres;
            true ->
                havings
        end,

    H1 = maps:put(Key, erlang:append_element(maps:get(Key, H), Expr), H),
    Meta1#{stack := [H1|T]}.


visit_select({atom, _, Table}, Meta = #{next_table := N}) ->
    {N, Table, Meta#{next_table := N+1}};
visit_select({call, _, {atom, _, select}, [E]}, Meta = #{next_table := N}) ->
    {Query, _, Meta1} = visit_subquery(none, E, Meta#{next_table := N+1}),
    {N, Query, Meta1};
visit_select({call, _, {atom, _, select}, [M, E]}, Meta = #{next_table := N}) ->
    {Query, _, Meta1} = visit_subquery(M, E, Meta#{next_table := N+1}),
    {N, Query, Meta1};
visit_select(E, _) ->
    throw({error, element(2,E), illegal_select_expression}).



visit_pattern({map, _, Fields}, Table, Meta) ->
    lists:foldl(
      fun ({map_field_exact, _, {atom, _, C}, F}, M) ->
              visit_map_pattern(F, {column, Table, C}, M);
          (P, _) ->
              throw({error, element(2, P), illegal_pattern})
      end,
      Meta,
      Fields);
visit_pattern(P, _, _) ->
    throw({error, element(2, P), illegal_pattern}).

visit_map_pattern({var, _, V}=Var, Column = {column, T, _}, Meta) ->
    set_aggregate(
      V,
      {ordsets:new(), ordsets:from_list([T])},
      bind_var(Var, Column, Meta));
visit_map_pattern(P, _, _) ->
    throw({error, element(2, P), illegal_pattern}).

visit_expression({call, _, {atom, _, c}, [{atom, _, C}]}, T, Meta) when is_integer(T), T > 0 ->
    A = {ordsets:new(), ordsets:from_list([T])},
    {{column, T, C}, A, add_aggregates(A, Meta)};
visit_expression({call, _, {atom, _, exists}, [E]}, T, Meta) ->
    {E1, A, Meta1} = visit_expression(E, T, Meta),
    {{expr, "(EXISTS(~s))", [E1]}, A, Meta1};
visit_expression({call, _, {atom, _, count}, [E]}, T, Meta=#{stack := [#{tables := Tables}|_]}) ->
    {E1, {_,A}, Meta1} = visit_expression(E, T, Meta),
    A1 =
        case ordsets:size(A) of
            0 ->
                ordsets:from_list([ N || {N, _} <- tuple_to_list(Tables)]);
            _ ->
                A
        end,
    {{expr, "count(~s)", [E1]}, {A1,A1}, add_aggregates({A1,A1}, Meta1)};

visit_expression({call, _, {atom, _, select}, [E]}, _, Meta) ->
    visit_subquery(none, E, Meta);
visit_expression({call, _, {atom, _, select}, [M, E]}, _, Meta) ->
    visit_subquery(M, E, Meta);
visit_expression({call, _, {atom, Line, param}, [Param]}, _, Meta = #{values := Values, params := Params}) ->
    Values1 = erlang:append_element(Values, Param),
    V = {value, tuple_size(Values1)},
    {V, {ordsets:new(), ordsets:new()}, Meta#{values := Values1, params := Params#{V => Line}}};
visit_expression({nil, _}=L, T, Meta) ->
    visit_array_expression(L, T, Meta);
visit_expression({cons,_,_,_}=L, T, Meta) ->
    visit_array_expression(L, T, Meta);
visit_expression({op, _, Op, X, {atom, _, null}}, T, Meta) when Op =:= '=:='; Op =:= '=/=' ->
    {X1, A, Meta1} = visit_expression(X, T, Meta),
    { {expr,
       case Op of
           '=:=' ->
               "(~s IS NULL)";
           '=/=' ->
               "(~s IS NOT NULL)"
       end,
       [X1]},
      A,
      Meta1};
visit_expression({op, _, Op, X, Y}, T, Meta) ->
    {X1, A1, Meta1} = visit_expression(X, T, Meta),
    {Y1, A2, Meta2} = visit_expression(Y, T, Meta1),

    {{expr,
      case Op of
          '==' ->
              "(~s = ~s)";
          '/=' ->
              "(~s <> ~s)";
          '>' ->
              "(~s > ~s)";
          '+' ->
              "(~s + ~s)";
          '++' ->
              "(~s || ~s)"
      end,
      [X1, Y1]},
     merge_aggregates(A1,A2),
     Meta2};
visit_expression({var, _, V}=Var, _, Meta) ->
    {E, Meta1 = #{stack := Stack, env := Env}} = resolve_var(Var, Meta),
    A =
        case ordsets:is_element(V, Env) of
            true ->
                {ordsets:new(), ordsets:new()};
            false ->
                get_aggregate(V, Stack)
        end,
    {E, A, add_aggregates(A, Meta1)};
visit_expression({integer, _, I}, _, Meta) ->
    {{integer, I}, {ordsets:new(), ordsets:new()}, Meta};
visit_expression(Expr, _, _Meta) ->
    throw({error, element(2, Expr), illegal_expression}).

visit_array_expression({nil, _}, _, Meta) ->
    {{expr, "ARRAY[]",  []},
     {ordsets:new(), ordsets:new()},
     Meta};
visit_array_expression({cons, _, Head, Tail}, T, Meta) ->
    {H1, A1, Meta1} = visit_expression(Head, T, Meta),
    {T1, A2, Meta2} = visit_array_tail(Tail, T, Meta1),

    {{expr, "ARRAY[~s~s]", [H1, T1]},
     merge_aggregates(A1,A2),
     Meta2}.

visit_array_tail({nil, _}, _, Meta) ->
    {{expr, "",  []},
     {ordsets:new(), ordsets:new()},
     Meta};
visit_array_tail({cons, _, Head, Tail}, T, Meta) ->
    {H1, A1, Meta1} = visit_expression(Head, T, Meta),
    {T1, A2, Meta2} = visit_array_tail(Tail, T, Meta1),

    {{expr, ",~s~s", [H1, T1]},
     merge_aggregates(A1,A2),
     Meta2}.



visit_order_expression({call, _, {atom, _, A}, [E]}, Meta)
  when A =:= asc; A =:= desc ->
    {E1, Meta1} = visit_column_expression(E, Meta),
    {{A, E1}, Meta1};
visit_order_expression({call, _, {atom, _, A}, [E, {atom, _, first}]}, Meta)
  when A =:= asc; A =:= desc ->
    {E1, Meta1} = visit_column_expression(E, Meta),
    {{A, E1, first}, Meta1};
visit_order_expression({call, _, {atom, _, A}, [E, {atom, _, last}]}, Meta)
  when A =:= asc; A =:= desc ->
    {E1, Meta1} = visit_column_expression(E, Meta),
    {{A, E1, last}, Meta1};
visit_order_expression(E, _) ->
    throw({error, element(2,E), illegal_order_expression}).


visit_column_expression(E, Meta) ->
    case visit_expression(E, none, Meta) of
        {{integer, _}, _, _} ->
            throw({error, element(2,E), integer_not_allowed});
        {E1, _, Meta1} ->
            {E1, Meta1}
    end.


make_index_map(L) ->
    maps:from_list(lists:zip(L, lists:seq(1, length(L)))).

add_table(Table, Meta = #{stack := [H = #{tables := Tables}|T]}) ->
    Meta#{stack := [H#{tables := erlang:append_element(Tables, Table)}|T]}.

is_bound(_, []) ->
    false;
is_bound(V, [#{vars := Vars}|T]) ->
    case maps:is_key(V, Vars) of
        true ->
            true;
        false ->
            is_bound(V, T)
    end.

bind_var({var, Line, V}, E, Meta = #{env := Env, stack := Stack = [H=#{vars := Vars, lines := Lines}|T]}) ->
    case atom_to_list(V) of
        [$_|_] ->
            Meta;
        _ ->
            case ordsets:is_element(V, Env) or is_bound(V, Stack) of
                true ->
                    throw({error, Line, {bound, V}});
                false ->
                    Meta#{stack := [H#{vars := Vars#{V => E}, lines := Lines#{V => Line}}|T]}
            end
    end.


resolve_var({var, Line, V}, Meta = #{env := Env, params := Params, stack := Stack}) ->
    case atom_to_list(V) of
        [$_|_] ->
            throw({error, Line, {unbound, V}});
        _ ->
            case ordsets:is_element(V, Env) of
                true ->
                    {{var, V},
                     Meta#{params := Params#{{var, V} => maps:get({var, V}, Params, Line)}}
                    };
                false ->
                    case lookup_var(V, Stack) of
                        error ->
                            throw({error, Line, {unbound, V}});
                        {ok, E, Stack1} ->
                            {E, Meta#{stack := Stack1}}
                    end
            end
    end.


lookup_var(_, []) ->
    error;
lookup_var(V, [H=#{vars := Vars, used := Used}|T]) ->
    case maps:find(V, Vars) of
        error ->
            case lookup_var(V, T) of
                error ->
                    error;
                {ok, E, T1} ->
                    {ok, E, [H|T1]}
            end;
        {ok, E} ->
            {ok, E, [H#{used := ordsets:add_element(V, Used)}|T]}
    end.


set_key(Key, Line, Value, Meta = #{stack := [H|T]}) ->
    case maps:is_key(Key, H) of
        true ->
            throw({error, Line, {once, Key}});
        false ->
            Meta#{stack := [H#{Key => Value}|T]}
    end.


get_aggregate(V, [#{aggregate := A}|T]) ->
    case maps:find(V, A) of
        error ->
            get_aggregate(V, T);
        {ok, X} ->
            X
    end.

set_aggregate(V, A, Meta = #{stack := [H = #{aggregate := As}|T]}) ->
    case atom_to_list(V) of
        [$_|_] ->
            Meta;
        _ ->
            Meta#{stack := [H#{aggregate := As#{V => A}}|T]}
    end.

add_aggregates(_, Meta = #{stack := []}) ->
    Meta;
add_aggregates(A1, Meta = #{stack := [H = #{aggregates := A2}|T]}) ->
    Meta#{stack := [H#{aggregates := merge_aggregates(A1,A2)}|T]}.

merge_aggregates({X1,Y1}, {X2,Y2}) ->
    {ordsets:union(X1,X2), ordsets:union(Y1,Y2)}.

is_aggregate({A, _}, #{stack := [#{tables := Tables}|_]}) ->
    case ordsets:size(
           ordsets:intersection(
             A,
             ordsets:from_list([N || {N, _} <- tuple_to_list(Tables)])))
    of
        0 ->
            false;
        _ ->
            true
    end.


build_query(Line, Call, C, CTEs, LC = {lc, _, _, [{generate,_,_}=G]}, #{stack := [Query], params := Params, values := Values, warnings := Warnings1}, Warnings) ->
    ParamList = maps:keys(Params),
    ParamIndex = make_index_map(ParamList),
    Params1 =
        [ case T of
              value ->
                  element(V, Values);
              _ ->
                  {T, maps:get(P, Params), V}
          end
          || P = {T, V} <- ParamList ],

    Used = check_query(CTEs, Query, ordsets:new()),

    Warnings2 =
        Warnings ++ Warnings1 ++
        [ {L, unused_param}
          || {{value,N},L} <- maps:to_list(Params),
             not ordsets:is_element(N, Used)
        ],

    SQL = format_query(CTEs, Query, ParamIndex),

    {setelement(
       4, LC,
       [erlang:append_element(
          G,
          erlang:append_element(
            Call,
            [erl_syntax:revert(C),
             {string, Line, lists:flatten(SQL)},
             erl_syntax:revert(erl_syntax:list(Params1))])
         )
       ]),
     Warnings2}.


check_query(CTEs, Query, Used) ->
    lists:foldl(
      fun (Q, U) ->
              check_query(Q, U)
      end,
      Used,
      [Q || {_, _, Queries} <- CTEs, Q <- Queries ] ++ [Query]).


check_query(Query = #{tables := Tables, outputs := Outputs, wheres := Wheres, havings := Havings}, Used) ->
    Used1 =
        lists:foldl(
          fun ({_, {from, F}}, U) ->
                  check_table(F, U);
              ({_, {join, J, Exprs}}, U)  ->
                  lists:foldl(
                    fun check_expression/2,
                    check_table(J, U),
                    Exprs)
          end,
          Used,
          tuple_to_list(Tables)),

    lists:foldl(
      fun check_expression/2,
      Used1,
      lists:append(
        [ maps:get(distinct, Query, []),
          case Outputs of
              _ when is_list(Outputs) ->
                  Outputs;
              _ when is_map(Outputs) ->
                  [C || {_,C} <- maps:to_list(Outputs)]
          end,
          tuple_to_list(Wheres),
          maps:get(group_by, Query, []),
          tuple_to_list(Havings),
          [ element(2,E) || E <- maps:get(order_by, Query, [])],
          case Query of
              #{offset := Offset} ->
                  [Offset];
              _ ->
                  []
          end,
          case Query of
              #{limit := Limit} ->
                  [Limit];
              _ ->
                  []
          end
        ])).

check_table(T, Used) when is_atom(T) ->
    Used;
check_table({subquery, CTEs, Q}, Used) ->
    check_query(CTEs, Q, Used).

check_expression({subquery, CTEs, Query}, Used) ->
    check_query(CTEs, Query, Used);
check_expression({expr, _, Args}, Used) ->
    lists:foldl(
      fun check_expression/2,
      Used,
      Args);
check_expression({value, V}, Used) ->
    ordsets:add_element(V, Used);
check_expression(_, Used) ->
    Used.


format_query(CTEs, Query, ParamIndex) ->
    [ [io_lib:format("WITH RECURSIVE ~w(", [Key]),
       string:join(
         [io_lib:format("~w", [F]) || F <- Fields],
         ", "),
       ") AS (",
       string:join([["(", format_query(Q, ParamIndex), ")"] || Q <- Queries], " UNION ALL "),
       ") "
      ]
      || {Key, Fields, Queries} <- CTEs ] ++ [format_query(Query, ParamIndex)].


format_query(Query = #{line := Line, tables := Tables, outputs := Outputs, wheres := Wheres, havings := Havings}, ParamIndex) ->
    case tuple_to_list(Tables) of
        [] ->
            throw({error, Line, no_table});
        [{NF, {from, First}}|Tables1] ->
            [ "SELECT ",
              case Query of
                  #{distinct := []} ->
                      "DISTINCT ";
                  #{distinct := Distinct} ->
                      ["DISTINCT ON (",
                       string:join(
                         [ format_expression(E, ParamIndex)
                           || E <- Distinct], ", "),
                       ") "
                      ];
                  #{} ->
                      []
              end,

              case Outputs of
                  [] ->
                      "1";
                  _ when is_map(Outputs), map_size(Outputs) =:= 0 ->
                      "1";
                  _ when is_list(Outputs) ->
                      string:join(
                        [ format_expression(C, ParamIndex)
                          || C <- Outputs ],
                        ", ");
                  _ when is_map(Outputs) ->
                      string:join(
                        [ io_lib:format(
                            "~s AS ~w",
                            [format_expression(C, ParamIndex), N])
                          || {N,C} <- maps:to_list(Outputs) ],
                        ", ")
              end,
              " FROM ",
              io_lib:format("~s AS T~b", [format_table(First, ParamIndex), NF]),

              [ case T of
                    {from, F} ->
                        io_lib:format(
                          " CROSS JOIN ~s AS T~b",
                          [format_table(F, ParamIndex), N]);
                    {join, J, Exprs} ->
                        [ io_lib:format(
                            " LEFT OUTER JOIN ~s AS T~b ON ",
                            [format_table(J, ParamIndex), N]),
                          case Exprs of
                              [] ->
                                  "TRUE";
                              _ ->
                                  string:join(
                                    [ format_expression(E, ParamIndex) || E <- Exprs], " AND ")
                          end
                        ]
                end
                || {N, T} <- Tables1
              ],

              case Wheres of
                  {} ->
                      [];
                  _ ->
                      [" WHERE ",
                       string:join(
                         [ format_expression(E, ParamIndex)
                           || E <- tuple_to_list(Wheres)
                         ], " AND ")
                      ]
              end,

              case Query of
                  #{group_by := GroupBy} ->
                      [" GROUP BY ",
                       string:join(
                         [ format_expression(E, ParamIndex)
                           || E <- GroupBy], ", ")
                       ];
                  #{} ->
                      []
              end,

              case Havings of
                  {} ->
                      [];
                  _ ->
                      [" HAVING ",
                       string:join(
                         [ format_expression(E, ParamIndex)
                           || E <- tuple_to_list(Havings)], " AND ")
                      ]
              end,

              case Query of
                  #{order_by := OrderBy} ->
                       [" ORDER BY ",
                        string:join(
                          [ [ format_expression(element(2,E), ParamIndex),
                              case element(1, E) of
                                  asc -> " ASC";
                                  desc -> " DESC"
                              end,
                              case E of
                                  {_,_} -> [];
                                  {_,_,first} -> " NULLS FIRST";
                                  {_,_,last} -> " NULLS LAST"
                              end
                            ]
                            || E <- OrderBy], ", ")
                       ];
                  _ ->
                      []
              end,

              case Query of
                  #{offset := Offset} ->
                      io_lib:format(" OFFSET (~s)", [format_expression(Offset, ParamIndex)]);
                  _ ->
                      []
              end,

              case Query of
                  #{limit := Limit} ->
                      io_lib:format(" LIMIT (~s)", [format_expression(Limit, ParamIndex)]);
                  _ ->
                      []
              end
            ];
        _ ->
            throw({error, Line, first_table_join})
    end.

format_table(T, _) when is_atom(T) ->
    io_lib:format("~w", [T]);
format_table({subquery, CTEs, Query}, ParamIndex) ->
    io_lib:format("LATERAL (~s)", [format_query(CTEs, Query, ParamIndex)]).


format_expression({subquery, CTEs, Query}, ParamIndex) ->
    io_lib:format("(~s)", [format_query(CTEs, Query, ParamIndex)]);
format_expression({expr, F, Args}, ParamIndex) ->
    io_lib:format(
      F,
      [ format_expression(A, ParamIndex)
        || A <- Args
      ]);
format_expression({column, T, C}, _) ->
    io_lib:format("T~b.~w", [T, C]);
format_expression({integer, I} , _) ->
    io_lib:format("~b", [I]);
format_expression({var, _} = V, ParamIndex) ->
    io_lib:format("$~b", [maps:get(V, ParamIndex)]);
format_expression({value, _} = V, ParamIndex) ->
    io_lib:format("$~b", [maps:get(V, ParamIndex)]).
