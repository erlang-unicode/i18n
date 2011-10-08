
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

export CC=$(shell icu-config --cc)
export CXX=$(shell icu-config --cxx)
export ICU_CFLAGS=$(shell icu-config --cflags) 
export ICU_CXXFLAGS=$(shell icu-config --cxxflags) 
export ICU_LDFLAGS=$(shell icu-config --ldflags) 
export VERSION=.$(shell date +%s)



all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) skip_deps=true doc

test:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

check_plt:
	@$(REBAR) check-plt

dialyzer:
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

