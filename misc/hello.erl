-module(hello).
-export([test/0]).
hello(world) -> ok;
hello(_) -> error.
test() ->
  ok = hello(world),
  error = hello(0),
  ok.
