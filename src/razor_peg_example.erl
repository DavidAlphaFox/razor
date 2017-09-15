-module(razor_peg_example).

-compile({parse_transform, razor_peg}).

-export([test/0]).

-rule(digit/1).

digit([H|T])
  when H >= $0, H =< $9 ->
    {ok, H - $0, T}.

int(S) -> int(0,S).

-rule(int/2).
int(Acc, S) ->
    {ok, N, S1} = digit(S),
    int(Acc*10+N, S1);
int(Acc, S) ->
    {ok, N, S1} = digit(S),
    {ok, Acc*10+N, S1}.


test() ->
    {ok, 1, ""} = int("1"),
    {ok, 12, ""} = int("12"),
    {ok, 12, "a"} = int("12a"),
    error = int("a"),
    error = int(""),
    ok.
