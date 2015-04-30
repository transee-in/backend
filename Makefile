APP    = transee
DEPS   = $(CURDIR)/deps
REBAR  = ./rebar
RELX   = ./relx
CONFIG = config/sys.config
env   := development

.PHONY: all get-deps compile release restart-release test

all: compile dev

deploy-node: get-deps compile release restart-release

deploy:
	@ssh transee 'cd ~/apps/transee && git pull && make deploy-node'

clean:
	@$(REBAR) clean skip_deps=true

get-deps:
	@$(REBAR) get-deps

compile: get-deps
	@$(REBAR) compile

compile-app:
	@$(REBAR) compile skip_deps=true

test:
	@$(REBAR) eu

release:
	@$(RELX) release

restart-release:
	@_rel/$(APP)/bin/$(APP) restart

start-release:
	@_rel/$(APP)/bin/$(APP) start

dev:
	@erl -pa deps/**/ebin ebin -config config/sys.config -s $(APP)
