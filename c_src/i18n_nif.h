/* vim: set filetype=cpp shiftwidth=4 tabstop=4 expandtab tw=80: */

/**
 *  =====================================================================
 *    Copyright 2011 Uvarov Michael 
 * 
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 * 
 *        http://www.apache.org/licenses/LICENSE-2.0
 * 
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 * 
 *  $Id$
 * 
 *  @copyright 2010-2011 Michael Uvarov
 *  @author Michael Uvarov <arcusfelis@gmail.com>
 *  =====================================================================
 */


#ifndef I18N_NIF
#define I18N_NIF        1


/* This parameter is for debug only */
#define I18N_INFO       0

#define I18N_STRING     1
#define I18N_COLLATION  1
#define I18N_SEARCH    	1
#define I18N_MESSAGE   	1
#define I18N_REGEX   	1
#define I18N_LOCALE   	1
#define I18N_DATE   	1
#define I18N_TRANS  	1
#define I18N_TEST   	1

/* 2^16, since we're using a 2-byte length header */
#define BUF_SIZE        65536 
#define STR_LEN         32768
#define LOCALE_LEN      255
#define ATOM_LEN        16

#define I18N_ERL_ONHEAP_BIN_LIMIT 64 // from erts/emulator/beam/erl_binary.h


#if I18N_INFO
#ifdef __GNUC__
#warning "HARD DEBUG IS ENABLED!"
#endif
#endif


#include "erl_nif.h"

extern "C" {
#include "cloner.h"
}

#if I18N_LOCALE
#include "unicode/uloc.h"
#endif

#include "unicode/ustring.h"
//#include "unicode/uchar.h"

#if I18N_STRING
#include "unicode/ubrk.h"
#include "unicode/unorm.h"
#endif

#if I18N_COLLATION
#include "unicode/ucol.h"
#include "unicode/coll.h"
#endif

#if I18N_SEARCH
#include "unicode/usearch.h"
#endif

#if I18N_MESSAGE
#include "unicode/umsg.h"
#include "unicode/msgfmt.h"
#include "unicode/messagepattern.h"
#endif

#include "unicode/utypes.h"

#if I18N_REGEX
#include "unicode/regex.h"
#endif

#if I18N_DATE
#include "unicode/ucal.h"
#include "unicode/calendar.h"
#include "unicode/timezone.h"
#endif

#if I18N_TRANS
#include "unicode/utrans.h"
#include "unicode/translit.h"
#include "unicode/uenum.h"
#endif

#include <string.h>

#define CHECK(ENV, X) \
    if (U_FAILURE(X)) return get_error_code(ENV, X, __FILE__, __LINE__); 

/* If error, run the destructor DEST */
#define CHECK_DEST(ENV, X, DEST) \
    if (U_FAILURE(X)) {DEST; return get_error_code(ENV, X, __FILE__, __LINE__);}


/* Divide by 2 */
#define TO_ULEN(X)   ((X) / sizeof(UChar))

/* Multiply by 2 */
#define FROM_ULEN(X) ((X) * sizeof(UChar))

#define ERROR(ENV, X) return get_error_code(ENV, X, __FILE__, __LINE__);





extern ERL_NIF_TERM res_error_term;

/* Allocated atoms */

extern ERL_NIF_TERM ATOM_TRUE, ATOM_FALSE;
extern ERL_NIF_TERM ATOM_EQUAL, ATOM_GREATER, ATOM_LESS;
extern ERL_NIF_TERM ATOM_OK;
extern ERL_NIF_TERM ATOM_ENDIAN;

extern ERL_NIF_TERM ATOM_COUNT;
extern ERL_NIF_TERM ATOM_RESOURCE;
extern ERL_NIF_TERM ATOM_SEARCH;


ERL_NIF_TERM reverse_list(ErlNifEnv *env, ERL_NIF_TERM list);


/* Define an interface for errors. */
ERL_NIF_TERM build_error(ErlNifEnv* env, ERL_NIF_TERM body, 
        const char *pszFile, long lLine);
ERL_NIF_TERM make_error(ErlNifEnv* env, const char* code, 
        const char *pszFile, long lLine);
ERL_NIF_TERM parse_error(ErlNifEnv* env, UErrorCode status, 
        const UParseError* e, 
        const char *pszFile, long lLine);
ERL_NIF_TERM list_element_error(ErlNifEnv* env, UErrorCode status, 
        const ERL_NIF_TERM list, int32_t num, 
        const char *pszFile, long lLine);

#define ERROR_TERM(ENV, X) \
    return build_error(ENV, X, __FILE__, __LINE__);
#define ERROR_STRING(ENV, X) \
    return make_error(ENV, X, __FILE__, __LINE__);
#define ERROR_PARSE(ENV, X, Y) \
    return parse_error(ENV, X, Y, __FILE__, __LINE__);
#define ERROR_ELEMENT(ENV, X, Y, Z) \
    return list_element_error(ENV, X, Y, Z, __FILE__, __LINE__);

#define CHECK_RES(ENV, RES) if (RES == NULL) ERROR_STRING(ENV, "bad_resource");

/** 
 * http://icu-project.org/apiref/icu4c/utypes_8h.html#a3343c1c8a8377277046774691c98d78c
 */
inline ERL_NIF_TERM get_error_code(ErlNifEnv* env, UErrorCode status, 
        const char *pszFile, long lLine) {
    return make_error(env, u_errorName(status), 
            pszFile, lLine);
}

ERL_NIF_TERM enum_to_term(ErlNifEnv* env, UEnumeration* en);
ERL_NIF_TERM enum_to_term(ErlNifEnv* env, StringEnumeration* en);


inline ERL_NIF_TERM string_to_term(ErlNifEnv* env, const UnicodeString& s) {
        ERL_NIF_TERM term;
        size_t len;
        const UChar* buf;
        unsigned char* bin;

        /* length in bytes */
        len = FROM_ULEN(s.length());
        buf = s.getBuffer();
        bin = enif_make_new_binary(env, len, &term);
        memcpy(bin, (const char*) buf, len);

        return term;
}


inline UnicodeString copy_binary_to_string(const ErlNifBinary& in) {
    /* Readonly-aliasing UChar* constructor. */
    return UnicodeString(
        (const UChar*) in.data,
        TO_ULEN(in.size));
}

inline UnicodeString binary_to_string(const ErlNifBinary& in) {
    /* Readonly-aliasing UChar* constructor. */
    return UnicodeString(
        false,
        (const UChar*) in.data,
        TO_ULEN(in.size));
}

/* Don't use in after calling the function! */
inline ERL_NIF_TERM enif_make_compact_binary(ErlNifEnv* env, ErlNifBinary* in)
{
    size_t len = in->size;
    if (len <= I18N_ERL_ONHEAP_BIN_LIMIT) {
        ERL_NIF_TERM out;
        unsigned char* buf = enif_make_new_binary(env, len, &out);
        memcpy((char*)buf, (const char *) (in->data), len);
        enif_release_binary(in);
        return out;
    }
    return enif_make_binary(env, in);
}

#if I18N_DATE
inline ERL_NIF_TERM calendar_to_double(ErlNifEnv* env, const UCalendar* cal) {
    UDate date;
    UErrorCode status = U_ZERO_ERROR;

    date = ucal_getMillis(cal, &status);
    CHECK(env, status);

    return enif_make_double(env, (double) date);
}
#endif

/**
 * see http://erlang.org/pipermail/erlang-questions/2011-December/063246.html
 */
inline ERL_NIF_TERM 
return_atom(ErlNifEnv* /*dest_env*/, ERL_NIF_TERM term)
{
    return term;
}

inline ERL_NIF_TERM bool_to_term(ErlNifEnv* env, UBool value) {
    return return_atom(env, value ? ATOM_TRUE : ATOM_FALSE); 
}



typedef const char* (*avail_fun)(int32_t);


ERL_NIF_TERM generate_available(ErlNifEnv* env, avail_fun fun, 
    int32_t i);

#define NIF_EXPORT(X) \
    ERL_NIF_TERM X(ErlNifEnv*, int, const ERL_NIF_TERM[]);

#endif
