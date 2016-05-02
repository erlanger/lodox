all: compile

.PHONY: compile doc

compile: ; @rebar3 compile

doc: html json

html: compile; @./make-html

json: compile; @./make-json
