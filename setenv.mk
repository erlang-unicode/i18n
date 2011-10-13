
export CC=$(shell icu-config --cc)
export CXX=$(shell icu-config --cxx)
export ICU_CFLAGS=$(shell icu-config --cflags) 
export ICU_CXXFLAGS=$(shell icu-config --cxxflags) 
export ICU_LDFLAGS=$(shell icu-config --ldflags) 
export I18N_BUILD_ID=.$(shell date +%s)
