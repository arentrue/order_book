REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
ELVIS := $(shell which elvis 2>/dev/null || which ./elvis)

.PHONY: rebar-update compile xref lint dialyze start devrel release clean distclean test

default: compile

rebar-update:
	$(REBAR) update

compile:
	$(REBAR) compile

xref:
	$(REBAR) xref

lint:
	$(ELVIS) rock

dialyze:
	$(REBAR) dialyzer

test:
	$(REBAR) eunit
	$(REBAR) ct

start:
	$(REBAR) run

devrel:
	$(REBAR) release

release:
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rf _build