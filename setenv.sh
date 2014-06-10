#!/bin/sh

export CC="$(icu-config --cc)"
export CXX="$(icu-config --cxx)"
export ICU_CFLAGS="$(icu-config --cflags)"
export ICU_CXXFLAGS="$(icu-config --cxxflags)"
export ICU_LDFLAGS="$(icu-config --ldflags)"
export I18N_BUILD_ID=".$(date +%s)"
