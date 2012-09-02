
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar  
#REBAR=./rebar -v 
#-include setenv.mk




all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) skip_deps=true doc

tests:
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



cover:
	export I18N_REBAR_COVER="true"; $(REBAR) skip_deps=true clean compile eunit
	rm -rf c_cov
	mkdir c_cov
	gcov -p -o c_src c_src/*.cpp
	mv *.gcov c_cov

