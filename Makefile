REBAR3 ?= rebar3

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
	$(REBAR3) eunit
