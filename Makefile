all: erl.mk
	test -f deps/erldocs/erldocs && rm deps/erldocs/erldocs || true
	$(MAKE) escript

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erldocs = https://github.com/erldocs/erldocs.git master

include erl.mk

# Your targets after this line.

clean: clean-ebin

distclean: clean clean-escript clean-deps
	$(if $(wildcard erl.mk), rm erl.mk   )
	$(if $(wildcard docs/), rm -rf docs/ )
.PHONY: distclean


debug: debug-app
