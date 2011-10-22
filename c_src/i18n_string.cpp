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
 *  @author Michael Uvarov <freeakk@gmail.com>
 *  =====================================================================
 */


#include "i18n_nif.h"


/**
 * i18n_string
 */
#if I18N_STRING

static ErlNifResourceType* iterator_type = 0;

static const Normalizer2* nfc_normalizer = 0;
static const Normalizer2* nfd_normalizer = 0;
static const Normalizer2* nfkc_normalizer = 0;
static const Normalizer2* nfkd_normalizer = 0;




/* Called from erl_nif. */
static void iterator_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
static void iterator_close(char* obj) 
{
    if (obj != NULL)
        ubrk_close((UBreakIterator*) obj);   
}
static char* iterator_clone(char* obj) 
{
    UErrorCode status = U_ZERO_ERROR;
    int32_t size = U_BRK_SAFECLONE_BUFFERSIZE;

    obj = (char*) ubrk_safeClone(
        (UBreakIterator*) obj,
        NULL,
        &size,
        &status 
    );
    if(U_FAILURE(status)) { 
        return NULL;
    } 
    return obj;
}

static int iterator_open(UBreakIterator * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &iterator_clone, &iterator_close);
} 








/**
 * Helpers
 */
static int parseIteratorType(const char * type) 
{
    return (!strcmp((char*) "grapheme", type)) ? UBRK_CHARACTER :
           (!strcmp((char*) "word", type))     ? UBRK_WORD      :
           (!strcmp((char*) "line", type))     ? UBRK_LINE      :
           (!strcmp((char*) "sentence", type)) ? UBRK_SENTENCE  :
            -1;
}


/**
 * NIFs
 */
inline void do_from_utf8(
    ErlNifBinary  in,
    ErlNifBinary& out, 
    int32_t& ulen,
    UErrorCode& status) 
{
    status = U_ZERO_ERROR;
    if (!enif_alloc_binary(FROM_ULEN(ulen), &out)) {
        status = U_MEMORY_ALLOCATION_ERROR;
        return;
    }

    u_strFromUTF8(
        (UChar*) out.data,       /* dest */
        ulen,                    /* capacity */
        &ulen,                   /* len of result */
        (char*)   in.data,       /* src */
        (int32_t) in.size,       /* len of src */
        &status);                /* error code */

    if (status == U_BUFFER_OVERFLOW_ERROR) {
        /* enlarge buffer if it was too small */
        enif_release_binary(&out);
        return;
    }

    if (FROM_ULEN(ulen) != out.size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(&out, FROM_ULEN(ulen));
    }
}

/* Convert utf8 to utf16 */
ERL_NIF_TERM from_utf8(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, out;
    int32_t ulen; 
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 1)
        return enif_make_badarg(env);

    /* First argument must be a binary */
    if(!enif_inspect_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }

    /* Convert a binary string in utf-8 to a binary string in utf-16. */
    ulen = in.size;
    do_from_utf8(in, out, ulen, status);
    if (status == U_BUFFER_OVERFLOW_ERROR) {
        do_from_utf8(in, out, ulen, status);
    }
    CHECK_DEST(env, status, 
        enif_release_binary(&out);
    );
    return enif_make_binary(env, &out);
}



inline void do_to_utf8(
    ErlNifBinary  in,
    ErlNifBinary& out, 
    int32_t& len,
    UErrorCode& status) 
{
    status = U_ZERO_ERROR;
    if (!enif_alloc_binary(len, &out)) {
        status = U_MEMORY_ALLOCATION_ERROR;
        return;
    }

    u_strToUTF8( 
        (char*) out.data,  /* dest */
        len, 
        &len, 
        (const UChar*) in.data, /* src */
        TO_ULEN(in.size),       /* len of src */
        &status);

    if (status == U_BUFFER_OVERFLOW_ERROR) {
        /* enlarge buffer if it was too small */
        enif_release_binary(&out);
        return;
    }

    if (len != (int32_t) out.size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(&out, len);
    }
}
/* Convert utf16 to utf8 */
ERL_NIF_TERM to_utf8(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, out;
    int32_t len; 
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 1)
        return enif_make_badarg(env);

    /* First argument must be a binary */
    if(!enif_inspect_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }

    len = in.size / sizeof(UChar);
    do_to_utf8(in, out, len, status);
    if (status == U_BUFFER_OVERFLOW_ERROR) {
        do_to_utf8(in, out, len, status);
    }
    CHECK_DEST(env, status, 
        enif_release_binary(&out);
    );
    return enif_make_binary(env, &out);
}

ERL_NIF_TERM endian(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM /*argv*/[]) {
    if (argc != 0)
        return enif_make_badarg(env);

    return ATOM_ENDIAN;
}

static ERL_NIF_TERM do_norm(ErlNifEnv* env, const ErlNifBinary& in, 
    const Normalizer2* norm)
{
    UnicodeString out_str;
    UErrorCode status = U_ZERO_ERROR;

    out_str = norm->normalize(binary_to_string(in), status);
    CHECK(env, status);

    return string_to_term(env, out_str);
}

/**
 * Normalization
 */

ERL_NIF_TERM to_nfc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return do_norm(env, in, nfc_normalizer);
}

ERL_NIF_TERM to_nfd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return do_norm(env, in, nfd_normalizer);
}

ERL_NIF_TERM to_nfkc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return do_norm(env, in, nfkc_normalizer);
}

ERL_NIF_TERM to_nfkd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return do_norm(env, in, nfkd_normalizer);
}



/**
 * Locale dependible functions
 */

typedef int32_t (*case_fun_ptr)(UChar *,
    int32_t,
    const UChar *,
    int32_t,
    const char *,
    UErrorCode *
    );

inline void do_case(
    ErlNifBinary  in,
    ErlNifBinary& out, 
    int32_t& ulen,
    case_fun_ptr fun,
    const char* locale,
    UErrorCode& status) 
{
    status = U_ZERO_ERROR;
    if (!enif_alloc_binary(FROM_ULEN(ulen), &out)) {
        status = U_MEMORY_ALLOCATION_ERROR;
        return;
    }
    ulen = fun(
            (UChar*) out.data,   /* src */
            ulen,                /* len of src */
            (UChar*) in.data, 
            TO_ULEN(in.size),
            locale, 
            &status);

    if (status == U_BUFFER_OVERFLOW_ERROR) {
        /* enlarge buffer if it was too small */
        enif_release_binary(&out);
        return;
    }

    if (FROM_ULEN(ulen) != out.size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(&out, FROM_ULEN(ulen));
    }
}

ERL_NIF_TERM to_upper(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, out;
    int32_t ulen; 
    UErrorCode status = U_ZERO_ERROR;
    char locale[LOCALE_LEN];

    if (argc != 2)
        return enif_make_badarg(env);

    if (!(enif_get_atom(env, argv[0], locale, LOCALE_LEN, ERL_NIF_LATIN1)
       && enif_inspect_binary(env, argv[1], &in))) {
        return enif_make_badarg(env);
    }

    ulen = TO_ULEN(in.size);
    do_case(in, out, ulen, u_strToUpper, (char*) locale, status);
    if (status == U_BUFFER_OVERFLOW_ERROR) {
        do_case(in, out, ulen, u_strToUpper, (char*) locale, status);
    }
    CHECK_DEST(env, status, 
        enif_release_binary(&out);
    );
    return enif_make_binary(env, &out);
}
ERL_NIF_TERM to_lower(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, out;
    int32_t ulen; 
    UErrorCode status = U_ZERO_ERROR;
    char locale[LOCALE_LEN];

    if (argc != 2)
        return enif_make_badarg(env);

    if (!(enif_get_atom(env, argv[0], locale, LOCALE_LEN, ERL_NIF_LATIN1)
       && enif_inspect_binary(env, argv[1], &in))) {
        return enif_make_badarg(env);
    }

    ulen = TO_ULEN(in.size);
    do_case(in, out, ulen, u_strToLower, (char*) locale, status);
    if (status == U_BUFFER_OVERFLOW_ERROR) {
        do_case(in, out, ulen, u_strToUpper, (char*) locale, status);
    }
    CHECK_DEST(env, status, 
        enif_release_binary(&out);
    );
    return enif_make_binary(env, &out);
}

inline void do_to_title(
    ErlNifBinary  in,
    ErlNifBinary& out, 
    int32_t& ulen,
    UBreakIterator* iter,
    const char* locale,
    UErrorCode& status) 
{
    status = U_ZERO_ERROR;
    if (!enif_alloc_binary(FROM_ULEN(ulen), &out)) {
        status = U_MEMORY_ALLOCATION_ERROR;
        return;
    }
    ulen = u_strToTitle(
            (UChar*) out.data,   /* src */
            ulen,                /* len of src */
            (UChar*) in.data, 
            TO_ULEN(in.size),
            iter,               /* Iterator */
            locale, 
            &status);

    if (status == U_BUFFER_OVERFLOW_ERROR) {
        /* enlarge buffer if it was too small */
        enif_release_binary(&out);
        return;
    }

    if (FROM_ULEN(ulen) != out.size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(&out, FROM_ULEN(ulen));
    }
}
ERL_NIF_TERM to_title(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, out;
    int32_t ulen; 
    char locale[LOCALE_LEN];
    const char* locptr;
    cloner* ptr; 
    UBreakIterator* iter; 
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 2)
        return enif_make_badarg(env);

    /* Second argument must be a binary */
    if(!enif_inspect_binary(env, argv[1], &in)) {
        return enif_make_badarg(env);
    }

    if (enif_get_atom(env, argv[0], locale, LOCALE_LEN, ERL_NIF_LATIN1)) {
        /* First element is an atom. */
        locptr = (const char*) locale;
        iter = NULL;

    } else if (enif_get_resource(env, argv[0], iterator_type, (void**) &ptr)) {
        /* First element is an iterator. */
        iter = (UBreakIterator*) cloner_get(ptr);
        CHECK_RES(env, iter);
            

        /* Iterator contains a locale name. Extract it. */
        locptr = ubrk_getLocaleByType((const UBreakIterator*) iter,
            ULOC_ACTUAL_LOCALE, /* locale type */
            &status);

        CHECK(env, status);
    } else {
        return enif_make_badarg(env);
    }  

    ulen = TO_ULEN(in.size);
    do_to_title(in, out, ulen, iter, locptr, status);
    if (status == U_BUFFER_OVERFLOW_ERROR) {
        do_to_title(in, out, ulen, iter, locptr, status);
    }
    CHECK_DEST(env, status, 
        enif_release_binary(&out);
    );
    return enif_make_binary(env, &out);

}
ERL_NIF_TERM len(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    cloner* ptr; 
    UBreakIterator* iter; 
    int count = -1, pos;
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 2)
        return enif_make_badarg(env);

    /* Last argument must be a binary */
    if (!(enif_inspect_binary(env, argv[1], &in)
       && enif_get_resource(env, argv[0], iterator_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }    
    iter = (UBreakIterator*) cloner_get(ptr);
    CHECK_RES(env, iter);

    if (iter == NULL) {
        return enif_make_badarg(env);
    }

    /* Do count */
    ubrk_setText(iter,
        (UChar *) in.data,
        TO_ULEN(in.size),
        &status);
    CHECK(env, status);

    for (pos = ubrk_first(iter);
         pos != UBRK_DONE;
         pos = ubrk_next(iter)) {
        count++;
    }

    return enif_make_int(env, count);
}
ERL_NIF_TERM split(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    cloner* ptr; 
    UBreakIterator* iter; 
    int len = -1, last, pos;
    UErrorCode status = U_ZERO_ERROR;
    ERL_NIF_TERM head, tail;
    UChar* bin; 
    UChar* text;
    

    if (argc != 2)
        return enif_make_badarg(env);

    /* Last argument must be a binary */
    if (!(enif_inspect_binary(env, argv[1], &in)
       && enif_get_resource(env, argv[0], iterator_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }    
    iter = (UBreakIterator*) cloner_get(ptr);
    CHECK_RES(env, iter);

    if (iter == NULL) {
        return enif_make_badarg(env);
    }
    text = (UChar*) in.data;

    ubrk_setText(iter, text, TO_ULEN(in.size), &status);
    CHECK(env, status);
    
    tail = enif_make_list(env, 0);
    pos = (int) ubrk_last(iter);

    while (pos) {
        last = pos;

        /* get the next elem. */
        pos = (int) ubrk_previous(iter);

        if (pos == UBRK_DONE)
            pos = 0;

        len = FROM_ULEN(last - pos);

        bin = (UChar*) enif_make_new_binary(env, len, &head);
        memcpy(bin, 
            (const char*) (text + pos), 
            len);
        tail = enif_make_list_cell(env, head, tail);
    };

    return tail;
}
ERL_NIF_TERM split_index(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    cloner* ptr; 
    UBreakIterator* iter; 
    int pos;
    UErrorCode status = U_ZERO_ERROR;
    ERL_NIF_TERM head, tail;
    

    if (argc != 2)
        return enif_make_badarg(env);

    /* Last argument must be a binary */
    if (!(enif_inspect_binary(env, argv[1], &in)
       && enif_get_resource(env, argv[0], iterator_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }    
    iter = (UBreakIterator*) cloner_get(ptr);
    CHECK_RES(env, iter);

    if (iter == NULL) {
        return enif_make_badarg(env);
    }

    ubrk_setText(iter, 
        (UChar*) in.data, 
        TO_ULEN(in.size), 
        &status);
    CHECK(env, status);
    
    tail = enif_make_list(env, 0);
    pos = (int) ubrk_last(iter);

    while ((pos != UBRK_DONE) && (pos != 0)) {
        head = enif_make_int(env, pos);
        tail = enif_make_list_cell(env, head, tail);

        /* get the next elem. */
        pos = (int) ubrk_previous(iter);
    };

    return tail;
}
ERL_NIF_TERM get_iterator(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out;
    int32_t atom_len; 
    char atom[LOCALE_LEN];
    int type;

    UErrorCode status = U_ZERO_ERROR;
    UBreakIteratorType iterType;
    UBreakIterator* iter; 
    cloner* res;

    if (argc != 2)
        return enif_make_badarg(env);

    /* Get iterator type */
    atom_len = enif_get_atom(env, argv[1], atom, LOCALE_LEN, ERL_NIF_LATIN1);
    if (!atom_len) {
        return enif_make_badarg(env);
    }
    /* atom_len is free. */

    /* If -1 then throw error (unknown type). */
      type = parseIteratorType((char*) atom);
    /* atom is free. */
    if (type == -1) {
        return enif_make_badarg(env);
    }
    iterType = (UBreakIteratorType) type;
    /* currb is free. */

    /* Get locale id: reuse atom and atom_len variables. */
    atom_len = enif_get_atom(env, argv[0], atom, LOCALE_LEN, ERL_NIF_LATIN1);
    if (!atom_len) {
        return enif_make_badarg(env);
    }


    iter = ubrk_open(iterType, 
            (char*) atom, /* locale */
            NULL,
            0, 
            &status);
    CHECK(env, status);

    if(!iter) { 
        return enif_make_badarg(env);
    } 

    res = (cloner*) enif_alloc_resource(iterator_type, sizeof(cloner));
    if (iterator_open(iter, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}



ERL_NIF_TERM iterator_locales(ErlNifEnv* env, int argc, const
    ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return generate_available(env, ubrk_getAvailable, ubrk_countAvailable());
}


int i18n_string_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    UErrorCode status = U_ZERO_ERROR;
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    iterator_type = enif_open_resource_type(env, NULL, "iterator_type",
        iterator_dtor, flags, NULL); 

    if (iterator_type == NULL) return 1;

    /* get normalizers */
    nfc_normalizer = Normalizer2::getInstance(NULL,
        "nfc",
        UNORM2_COMPOSE,
        status);
    if(U_FAILURE(status)) return 2;

    nfd_normalizer = Normalizer2::getInstance(NULL,
        "nfc",
        UNORM2_DECOMPOSE,
        status);
    if(U_FAILURE(status)) return 2;

    nfkc_normalizer = Normalizer2::getInstance(NULL,
        "nfkc",
        UNORM2_COMPOSE,
        status);
    if(U_FAILURE(status)) return 2;

    nfkd_normalizer = Normalizer2::getInstance(NULL,
        "nfkc",
        UNORM2_DECOMPOSE,
        status);
    if(U_FAILURE(status)) return 2;

    return 0;
}

#endif








