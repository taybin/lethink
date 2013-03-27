REBAR=`which rebar || printf ./rebar`
REPO=lethink
all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

ct:
	@$(REBAR) skip_deps=true ct

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit ct

clean:
	@$(REBAR) clean

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = .$(REPO)_combo_dialyzer.plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) deps ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) deps ebin

dialyzer: compile
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin
