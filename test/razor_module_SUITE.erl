-module(razor_module_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_range_list,
     test_bitset,
     test_regex,
     test_url_regex,
     test_utf8_regex,
     test_nfa,
     test_thompson_nfa,
     test_dfa,
     test_tagged_nfa,
     test_url_rule,
     test_url_examples].

test_range_list(Config) ->
    test_module(razor_range_list, Config).

test_bitset(Config) ->
    test_module(razor_bitset, Config).

test_regex(Config) ->
    test_module(razor_regex, Config).

test_url_regex(Config) ->
    test_module(razor_url_regex, Config).

test_utf8_regex(Config) ->
    test_module(razor_utf8_regex, Config).

test_nfa(Config) ->
    test_module(razor_nfa, Config).

test_thompson_nfa(Config) ->
    test_module(razor_thompson_nfa, Config).

test_dfa(Config) ->
    test_module(razor_dfa, Config).

test_tagged_nfa(Config) ->
    test_module(razor_tagged_nfa, Config).

test_url_rule(Config) ->
    test_module(razor_url_rule, Config).

test_url_examples(Config) ->
    test_module(razor_url_example, Config).

test_module(Mod, Config) ->
    Mod:test().
