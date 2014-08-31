all: erl.mk | ensure

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erldocs = https://github.com/erldocs/erldocs.git master

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard erl.mk), rm erl.mk   )
	$(if $(wildcard erldocs_other), rm erldocs_other)
.PHONY: distclean

ensure:
	test -f deps/erldocs/erldocs && rm deps/erldocs/erldocs || true
	test -f $(APP) && rm $(APP) || true

all: escript


debug: ERLCFLAGS += +export_all +debug_info
debug: all
	erl -pa ebin/ -pa deps/*/ebin/ -eval 'c:l($(APP)).'
