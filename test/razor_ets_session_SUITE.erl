-module(razor_ets_session_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_session].

init_per_testcase(_, Config) ->
    {ok, Pid} = razor_ets_session:start(),
    [{pid, Pid}|Config].

end_per_testcase(_, Config) ->
    exit(?config(pid, Config), kill).

test_session(_Config) ->
    true = razor_ets_session:new_session(<<"1">>),
    false = razor_ets_session:new_session(<<"1">>),
    Empty = #{},
    {ok, Empty} = razor_ets_session:get_session(<<"1">>),
    false = razor_ets_session:save_session(<<"0">>, #{}),
    Session = #{a => 1},
    true = razor_ets_session:save_session(<<"1">>, Session),
    {ok, Session} = razor_ets_session:get_session(<<"1">>).
