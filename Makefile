
live_ui:
	cd ui; npm run dev

.PHONY: deps
deps:
	./rebar3 deps

.PHONY: compile
compile:
	./rebar3 compile

.PHONY: shell
shell:
	./rebar3 shell

.PHONY: dialyze
dialyze:
	./rebar3 dialyzer

.PHONY: test
test:
	rm -rf _build/test/cover
	./rebar3 eunit

erlfmt:
	./rebar3 fmt -w

erlfmt_check:
	./rebar3 fmt --check

