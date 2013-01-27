REBAR := $(shell which rebar 2>/dev/null || echo ./rebar)
REBAR_URL := https://github.com/downloads/basho/rebar/rebar

.PHONY: compile test

all: compile

compile: $(REBAR)
	$(REBAR) get-deps compile

test: $(REBAR)
	$(REBAR) -C rebar.test.config get-dep compile
	$(REBAR) -C rebar.test.config eunit -v skip_deps=true

clean:
	$(REBAR) clean

./rebar:
	erl -noshell -s inets start -s ssl start \
        -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar
