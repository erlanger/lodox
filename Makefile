all: compile

.PHONY: compile doc

compile: ; @rebar3 compile

docs: html json

html: compile; @./bin/make-html

json: compile; @./bin/make-json
