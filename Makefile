all: compile

.PHONY: compile doc

compile: ; @rebar3 compile

doc: compile; @./make-doc
