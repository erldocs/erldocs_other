REBAR3 ?= rebar3
SHELL := /bin/bash

.PHONY: all test

all:
	$(REBAR3) compile

distclean:
	$(if $(wildcard _build), rm -rf _build)
	$(if $(wildcard docs/), rm -rf docs/ )
	$(if $(wildcard erl_crash.dump), rm erl_crash.dump)

dialyze: app
	dialyzer --src src/ --plt ~/.dialyzer_plt --no_native  -Werror_handling -Wrace_conditions -Wunmatched_returns -Wunderspecs

test:
	[[ 0 == "$$(./meta_forks.escript test/meta_DATA/single 2>/dev/null)" ]]
	[[ 26 == "$$(./meta_forks.escript test/meta_DATA/lager 2>/dev/null)" ]]
	$(REBAR3) eunit
