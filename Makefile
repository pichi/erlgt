## -----------------------------------------------------------------------------
##
## The MIT License (MIT)
##
## Copyright (c) 2015 Hynek Vychodil
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.
##
## -----------------------------------------------------------------------------

ERLFLAGS = -pa $(CURDIR)/deps/*/ebin -pa $(CURDIR)/ebin -pa $(CURDIR)/.eunit

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================

ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(shell wget https://github.com/rebar/rebar/wiki/rebar)
REBAR=./rebar	
$(shell chmod +x $(REBAR))
endif

# =============================================================================
# Rules to build the system
# =============================================================================

.PHONY: all compile doc clean test shell distclean update-deps

all: deps compile test

deps:
	@$(REBAR) get-deps compile

update-deps:
	@$(REBAR) update-deps compile

compile:
	@$(REBAR) skip_deps=true compile

doc:
	@$(REBAR) skip_deps=true doc

eunit: compile
	@$(REBAR) skip_deps=true eunit

test: eunit

shell: deps
	- @$(REBAR) skip_deps=true eunit suite=gen_digraph
	- @$(ERL) $(ERLFLAGS)

clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	@$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rvf $(CURDIR)/deps

rebuild: distclean deps compile
