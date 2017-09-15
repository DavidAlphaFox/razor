-module(razor_regex).

-compile({parse_transform, razor_peg}).

-export([test/0]).

-export(
   [ parse/1,
     simplify/1
   ]).

parse(S) ->
    case re(S) of
        {ok, Pattern, []} ->
            {ok, simplify(Pattern)};
        _ ->
            error
    end.

-rule(re/1).

re(S) ->
    {ok, H, S1} = seq(S),
    {ok, T, S2} = re_tail(S1),
    {ok, [H|T], S2}.

-rule(re_tail/1).

re_tail([$||S]) ->
    re(S);
re_tail(S) ->
    {ok, [], S}.

-rule(seq/1).

seq(S) ->
    {ok, H, S1} = basic(S),
    {ok, T, S2} = seq(S1),
    {ok, [H|T], S2};
seq(S) ->
    {ok, [], S}.

-rule(basic/1).

basic(S) ->
    {ok, E, S1} = elem(S),
    basic_tail(E, S1).

-rule(basic_tail/2).

basic_tail(E, [$+|S]) ->
    basic_tail({repeat, 1, infinity, E}, S);
basic_tail(E, [$*|S]) ->
    basic_tail({repeat, 0, infinity, E}, S);
basic_tail(E, [$?|S]) ->
    basic_tail({repeat, 0, 1, E}, S);
basic_tail(E, [${|S]) ->
    {ok, X, [$,|S1]} = integer(0, S),
    {ok, Y, [$}|S2]} = repeat(S1),
    {ok, {repeat, X, Y, E}, S2};
basic_tail(E, S) ->
    {ok, E, S}.

-rule(repeat/1).

repeat([C|_]=S) when $0 =< C, C =< $9 ->
    integer(0, S);
repeat(S) ->
    {ok, infinity, S}.

-rule(integer/2).

integer(N, [C|S]) when $0 =< C, C =< $9 ->
    integer(N*10 + C - $0, S);
integer(N, S) ->
    {ok, N, S}.

-rule(elem/1).

elem([C|S])
  when C =/= $(, C =/= $), C =/= $[, C =/= $],
       C =/= $+, C =/= $*, C =/= $?, C =/= $|,
       C =/= ${, C =/= $}, C =/= $., C =/= $\\ ->
    {ok, C, S};
elem([$\\, C|S])
  when C =:= $(; C =:= $); C =:= $[; C =:= $];
       C =:= $+; C =:= $*; C =:= $?; C =:= $|;
       C =:= ${; C =:= $}; C =:= $.; C =:= $\\ ->
    {ok, C, S};
elem([$(|S]) ->
    {ok, E, [$)|S1]} = re(S),
    {ok, {group, E}, S1};
elem([$.|S]) ->
    {ok, '.', S};
elem([$[|S]) ->
    {ok, E, [$]|S1]} = set(S),
    {ok, {set, razor_range_list:from_list(E)}, S1}.

-rule(set/1).

set(S) ->
    {ok, H, S1} = range(S),
    {ok, T, S2} = set(S1),
    {ok, [H|T], S2};
set(S) ->
    {ok, [], S}.

-rule(range/1).

range(S) ->
    {ok, C1, [$-|S1]} = char(S),
    {ok, C2, S2} = char(S1),
    {ok, {C1, C2}, S2};
range(S) ->
    {ok, C, S1} = char(S),
    {ok, {C, C}, S1}.

-rule(char/1).

char([C|S])
  when C =/= $\\, C =/= $[, C =/= $] ->
    {ok, C, S};
char([$\\, C|S])
  when C =:= $\\, C =:= $[, C =:= $] ->
    {ok, C, S}.


simplify(RE) ->
    RE1 = lists:append([simplify_seq(Seq) || Seq <- RE]),
    Set =
        razor_range_list:from_list(
          lists:append(
            [case Seq of
                 [{set, Set}] ->
                     Set;
                 [C] when is_integer(C) ->
                     [{C,C}];
                 _ ->
                     []
             end
             || Seq <- RE1])),

    Others =
        [Seq
         || Seq <- RE1,
            case Seq of
                [{set, _}] ->
                    false;
                [C] when is_integer(C) ->
                    false;
                _ ->
                    true
            end
        ],

    case Set of
        [] ->
            Others;
        [{X,X}] ->
            [[X]|Others];
        _ ->
            [[{set, Set}]|Others]
    end.


simplify_seq([{group, G}]) ->
    simplify(G);
simplify_seq(Seq) ->
    Seq1 = [ simplify_elem(E) || E <- Seq ],
    case lists:any(
           fun ({group, []}) -> true;
               ({set, []}) -> true;
               (_) -> false
           end,
           Seq1)
    of
        true ->
            [];
        false ->
            [Seq1]
    end.

simplify_elem({repeat, X, Y, E}) ->
    {repeat, X, Y, simplify_elem(E)};
simplify_elem({group, RE}) ->
    case simplify(RE) of
        [[E]] ->
            E;
        G ->
            {group, G}
    end;
simplify_elem({set, [{X,X}]}) ->
    X;
simplify_elem(E) ->
    E.


test(re) ->
    {ok, [[$a],[$b],[$c]], []} = re("a|b|c"),
    {ok, [[$a,$b,$c]], []} = re("abc"),
    {ok, [[{repeat, 1, infinity, $a}]], []} = re("a+"),
    {ok, [[{repeat, 0, 1, $a}]], []} = re("a{0,1}"),
    {ok, [[{repeat, 0, infinity, $a}]], []} = re("a{0,}"),
    {ok, [[{group, [[$a]]}]], []} = re("(a)"),
    {ok, [[{set, [{$0,$9},{$a,$a}]}]], []} = re("[0-9a]"),
    {ok, [[{set, [{$0,$9}]}]], []} = re("[0-56-9]"),
    {ok, [[{set, [{$0,$9}]}]], []} = re("[0-65-9]"),
    ok;
test(parse) ->
    {ok, [[{set, [{$0,$9}]}]]} = parse("[0-4]|5|[6-9]"),
    {ok, [[{set, [{$0,$9},{$a,$a}]}]]} = parse("[0-4]|5|[6-9]|a"),
    {ok, [[{set, [{$1,$1},{$3,$3},{$5,$5},{$a,$a},{$c,$c},{$e,$e}]}]]} = parse("(a|c|e)|(1|3|5)"),
    {ok, [[$a, $5]]} = parse("a[5-5]"),
    {ok, []} = parse("a[]"),
    {ok, [[$a, $b]]} = parse("(a)b"),
    ok.
test() ->
    test(re),
    test(parse),
    ok.
