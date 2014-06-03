REBAR := ./rebar
REBAR_URL := https://github.com/rebar/rebar/wiki/rebar
ERL       ?= erl

.PHONY: compile test

all: compile

compile: $(REBAR)
	$(REBAR) get-deps compile

test: $(REBAR)
	$(REBAR) -C rebar.test.config get-dep compile
	$(REBAR) -C rebar.test.config eunit -v skip_deps=true

clean: $(REBAR)
	$(REBAR) clean

./rebar:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
	  -s init stop
	chmod +x ./rebar
