all: erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erldocs = https://github.com/erldocs/erldocs.git master

ERLCFLAGS += +debug_info

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard erl.mk), rm erl.mk   )
.PHONY: distclean
