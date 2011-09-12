UNAME := $(shell uname)
ERL_INC=$(shell erl -noinput -noshell -eval 'io:format(code:root_dir() ++ "/usr/include/"), halt().')

SRC_DIR=./
DEST_DIR=./../priv/

# --cflags 
CC:=$(shell icu-config --cc  --cppflags-searchpath --ldflags)

# OS X flags.
ifeq ($(UNAME),Darwin)
GCCFLAGS = -O3 -fPIC -bundle -flat_namespace -undefined suppress -fno-common -Wall
endif

ifeq ($(UNAME),Linux)
GCCFLAGS = -O3 -fPIC -shared -fno-common -Wall
endif


compile:
	$(CC) -I $(ERL_INC) $(GCCFLAGS) -o $(DEST_DIR)i18n_nif.so \
	 $(SRC_DIR)i18n_nif.cpp \
	 $(SRC_DIR)cloner.c
	
clean:
	-@rm $(DEST_DIR)*.so
