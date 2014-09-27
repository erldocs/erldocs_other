all: escript | erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erldocs = https://github.com/erldocs/erldocs.git master

-include erl.mk
# Your targets after this line.
.PHONY: clean distclean debug

clean: clean-ebin

distclean: clean clean-escript clean-deps
	$(if $(wildcard erl.mk), rm erl.mk   )
	$(if $(wildcard docs/), rm -rf docs/ )

debug: debug-app
