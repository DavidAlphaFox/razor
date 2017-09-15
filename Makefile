EPGSQL_SRCS=$(wildcard lib/epgsql/src/*.erl)
EPGSQL_HRLS=$(wildcard lib/epgsql/include/*.erl)
EPGSQL_BEAMS=$(EPGSQL_SRCS:lib/epgsql/src/%.erl=lib/epgsql/ebin/%.beam)

SRCS=$(wildcard src/*.erl)
BEAMS=$(SRCS:src/%.erl=ebin/%.beam)

all: $(EPGSQL_BEAMS) $(BEAMS)

lib/epgsql/ebin/%.beam: lib/epgsql/src/%.erl $(EPGSQL_HRLS)
	erlc -I lib/epgsql/include -o lib/epgsql/ebin/ "$<"

ebin/%.beam: src/%.erl
	erlc +debug_info -pa ebin -o ebin "$<"

ebin/razor_demo.beam: ebin/razor_url_dispatch.beam ebin/razor_db.beam
ebin/razor_example.beam: ebin/razor_url_dispatch.beam
ebin/razor_peg_example.beam: ebin/razor_peg.beam
ebin/razor_db_examples.beam: ebin/razor_db.beam
ebin/razor_url_example.beam: ebin/razor_url_dispatch.beam
ebin/razor_url_rule.beam: ebin/razor_peg.beam
ebin/razor_regex.beam: ebin/razor_peg.beam
ebin/razor_url_dispatch.beam: ebin/razor_url_rule.beam ebin/razor_regex.beam ebin/razor_nfa.beam ebin/razor_tagged_nfa.beam ebin/razor_dfa.beam ebin/razor_utf8_regex.beam ebin/razor_url_regex.beam ebin/razor_group_map.beam ebin/razor_range_list.beam ebin/razor_utils.beam ebin/razor_thompson_nfa.beam
