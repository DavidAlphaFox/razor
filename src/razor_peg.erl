-module(razor_peg).

-export([parse_transform/2]).


args(L,N) ->
    [{var,L, list_to_atom([$V|integer_to_list(I)])}|| I <- lists:seq(1,N)].


rewrite_var([H|T],N) ->
    [rewrite_var(H,N)|rewrite_var(T,N)];
rewrite_var({var,L,V},N) ->
    {var,L,
     case atom_to_list(V) of
         [$_|_] ->
             V;
         S ->
            list_to_atom("V"++integer_to_list(N)++S)
     end};
rewrite_var({cons,L,H,T},N) ->
    {cons,L,rewrite_var(H,N),rewrite_var(T,N)};
rewrite_var({tuple,L,Elems},N) ->
    {tuple,L,rewrite_var(Elems,N)};
rewrite_var({match,L,P,E},N) ->
    {match,L,rewrite_var(P,N),rewrite_var(E,N)};
rewrite_var({call,L,F,A},N) ->
    {call,L,rewrite_var(F,N),rewrite_var(A,N)};
rewrite_var({remote,L,M,F},N) ->
    {remote,L,rewrite_var(M,N),rewrite_var(F,N)};
rewrite_var({op,L,Op,A},N) ->
    {op,L,Op,rewrite_var(A,N)};
rewrite_var({op,L,Op,A,B},N) ->
    {op,L,Op,rewrite_var(A,N),rewrite_var(B,N)};
rewrite_var({'case',L,E,C},N) ->
    {'case',L,rewrite_var(E,N),rewrite_var(C,N)};
rewrite_var({clause,L,P,G,E},N) ->
    {clause,L,rewrite_var(P,N),rewrite_var(G,N),rewrite_var(E,N)};
rewrite_var(X,_N) ->
    X.


is_always_match([],[]) ->
    true;
is_always_match([{var,_,_}|T],[]) ->
    is_always_match(T,[]);
is_always_match(_,_) ->
    false.


%% transform
%%   PATTERN = f(),
%%   ...
%% to
%%   case f() of
%%     PATTERN ->
%%       TRANSFORM(...)
%%     _ ->
%%       error
%%   end

transform_exprs([],_) ->
    [];
transform_exprs([{match,L,P,{call,_,{atom,_,F},A}=Call}=H|T],Rules) ->
    case lists:member({F,length(A)},Rules) of
        false ->
            [H|transform_exprs(T,Rules)];
        true ->
            [{'case',L,
              Call,
              [{clause,L,[P],[],transform_exprs(T,Rules)},
               {clause,L,[{var,L,'_'}],[],[{atom,L,error}]}]
             }]
    end;
transform_exprs([H|T],Rules) ->
    [H|transform_exprs(T,Rules)].


%% transform
%%   f(PATTERNS) when GUARDS ->
%%      EXPRS;
%%   ...
%% to
%%   f(ARGS) ->
%%     case(
%%       case {ARGS} of
%%         {PATTERN} when GUARDS ->
%%           TRANSFORM_EXPRS(EXPRS)
%%         _ ->
%%           error
%%       end)
%%     of
%%       error ->
%%         TRANSFORM(...)
%%       V0 ->
%%         V0
%%     end.

transform_clauses([],_,_,Line,_) ->
    [{atom,Line,error}];
transform_clauses([{clause,L,P,G,E}|T],Acc,Args,Line,Rules) ->
    [{'case',L,
      {'case',L,
       {tuple,L,Args},
       [{clause,L,[{tuple,L,rewrite_var(P,Acc)}],rewrite_var(G,Acc),
         transform_exprs(rewrite_var(E,Acc),Rules)}
        |case is_always_match(P,G) of
             true ->
                 [];
             false ->
                 [{clause,L,[{var,L,'_'}],[],[{atom,L,error}]}]
         end]
      },
      [{clause,L,[{atom,L,error}],[],
        transform_clauses(T,Acc+1,Args,Line,Rules)},
       {clause,L,[{var,L,'V0'}],[],[{var,L,'V0'}]}]
     }].


transform_forms([],_Rules) ->
    [];
transform_forms([{function,L,F,A,Clauses}|T],Rules) ->
    [{function,L,F,A,
      case lists:member({F,A},Rules) of
          false ->
              Clauses;
          true ->
              Args = args(L,A),
              [{clause,L,Args,[],
                transform_clauses(Clauses,0,Args,L,Rules)}]
      end
     }|transform_forms(T,Rules)];
transform_forms([{attribute, _, rule, _}|T], Rules) ->
    transform_forms(T,Rules);
transform_forms([H|T], Rules) ->
    [H|transform_forms(T,Rules)].


parse_transform(Forms, _Options) ->
    Rules = [Rule || {attribute, _, rule, Rule} <- Forms],

    Forms1 = transform_forms(Forms, Rules),

    io:format(
      "After ~w:~n~s~n",
      [?MODULE, erl_prettypr:format(erl_syntax:form_list(Forms1))]),

    Forms1.
