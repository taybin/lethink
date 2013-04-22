PROJECT=lethink
REBAR=`which rebar || printf ./rebar`

# Main

.PHONY: all
all: get-deps compile

.PHONY: clean
clean:
	@$(REBAR) clean

.PHONY: distclean
distclean: clean-docs
	rm -fr deps/

.PHONY: get-deps
get-deps:
	@$(REBAR) get-deps

.PHONY: compile
compile:
	@$(REBAR) compile

# Docs

docs: clean-docs
	@$(REBAR) doc skip_deps=true

clean-docs:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info

# Tests.

deps/proper:
	@$(REBAR) -C rebar.tests.config get-deps
	cd deps/proper && $(REBAR) compile

tests: clean deps/proper all eunit ct

eunit: all
	@$(REBAR) eunit skip_deps=true

ct: all
	@$(REBAR) ct skip_deps=true

# Dialyzer

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = .$(PROJECT).plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) deps ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) deps ebin

dialyzer: compile
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin

# xref

xref:
	@$(REBAR) xref skip_deps=true
