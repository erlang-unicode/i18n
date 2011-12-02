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
#define UBRK_MAX (UBRK_SENTENCE + 1)
#define UBRK_WORD_ONLY (UBRK_WORD + UBRK_MAX)
#define TO_UBRK(X) ((UBreakIteratorType) ((X) % UBRK_MAX))

typedef struct {
    cloner res;
    int (*skip_elem)(int32_t);
} cloner_break;

int elem_word_none(int32_t breakType)
{
    return ((breakType == UBRK_WORD_NONE) ? 1 : 0);
}

static int parseIteratorType(const char * type) 
{
    return (!strcmp((char*) "grapheme", type)) ? UBRK_CHARACTER :
           (!strcmp((char*) "word", type))     ? UBRK_WORD      :
           (!strcmp((char*) "line", type))     ? UBRK_LINE      :
           (!strcmp((char*) "sentence", type)) ? UBRK_SENTENCE  :
           (!strcmp((char*) "word_only", type))     
                    ? UBRK_WORD_ONLY :
            -1;
}

/**
 * Is current element in this iterator valid?
 * If result is false, the element will be skipped.
 * Used for word_only. 
 */
inline int is_valid_elem(cloner* res, UBreakIterator* iter)
{
    cloner_break* res_brk = (cloner_break*) res;
    int32_t type;

    if (res_brk->skip_elem == NULL)
        return 1;

    type = ubrk_getRuleStatus(iter);
    return !((res_brk->skip_elem)(type));
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

    if (U_FAILURE(status)) {
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
    CHECK(env, status);
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

    if (U_FAILURE(status)) {
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
    CHECK(env, status);
    return enif_make_binary(env, &out);
}

ERL_NIF_TERM endian(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM /*argv*/[]) {
    if (argc != 0)
        return enif_make_badarg(env);

    return ATOM_ENDIAN;
}

static inline ERL_NIF_TERM 
compare_result_to_atom(int32_t res)
{
    return (!res)  ? ATOM_EQUAL :
           (res>0) ? ATOM_GREATER : ATOM_LESS;
}

ERL_NIF_TERM case_compare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, in2;
    char locale[LOCALE_LEN];
    UErrorCode status = U_ZERO_ERROR;
    uint32_t options = U_COMPARE_IGNORE_CASE;
    int32_t res;

    if (argc != 3)
        return enif_make_badarg(env);

    if (!(enif_get_atom(env, argv[0], locale, LOCALE_LEN, ERL_NIF_LATIN1)
       && enif_inspect_binary(env, argv[1], &in)
       && enif_inspect_binary(env, argv[2], &in2))) {
        return enif_make_badarg(env);
    }

    /* Is the locale tr? */
    if ((locale[0] == 't') && (locale[1] == 'r')) 
        options |= U_FOLD_CASE_EXCLUDE_SPECIAL_I;

    /*res = u_strCaseCompare(*/
    res = unorm_compare(
            (const UChar *) in.data,
            TO_ULEN(in.size),
            (const UChar *) in2.data,
            TO_ULEN(in2.size),
            options,
            &status);
    CHECK(env, status);

    return compare_result_to_atom(res);
}

ERL_NIF_TERM non_case_compare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, in2;
    UErrorCode status = U_ZERO_ERROR;
    int32_t res;

    if (argc != 2)
        return enif_make_badarg(env);

    if (!(enif_inspect_binary(env, argv[0], &in)
       && enif_inspect_binary(env, argv[1], &in2))) {
        return enif_make_badarg(env);
    }

    /* Case-sensitive comparison in code unit order, and the input strings are
     * quick-checked for FCD. */

    res = unorm_compare(
        (const UChar *) in.data,
        TO_ULEN(in.size),
        (const UChar *) in2.data,
        TO_ULEN(in2.size),
        U_FOLD_CASE_DEFAULT,
        &status);
    CHECK(env, status);

    return compare_result_to_atom(res);
}


static inline void 
do_norm( 
    ErlNifBinary  in,
    ErlNifBinary& out, 
    int32_t& ulen,
    UNormalizationMode mode,
    UErrorCode& status) 
{
    status = U_ZERO_ERROR;
    if (!enif_alloc_binary(FROM_ULEN(ulen), &out)) {
        status = U_MEMORY_ALLOCATION_ERROR;
        return;
    }

    /* set a new ulen */
    ulen = unorm_normalize(
        (const UChar *) in.data,
        TO_ULEN(in.size),
        mode,
        0,
        (UChar *) out.data,
        ulen,
        &status);

    if (U_FAILURE(status)) {
        /* release the memory in one place */
        enif_release_binary(&out);
        return;
    }

    if (FROM_ULEN(ulen) != out.size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(&out, FROM_ULEN(ulen));
    }

}

ERL_NIF_TERM norm(ErlNifEnv* env, ErlNifBinary in, 
    UNormalizationMode mode)
{
    int32_t ulen;
    ErlNifBinary out;
    UErrorCode status;

    ulen = TO_ULEN(in.size);
    do_norm(in, out, ulen, mode, status);
    if (status == U_BUFFER_OVERFLOW_ERROR) {
        do_norm(in, out, ulen, mode, status);
    }
    CHECK(env, status);
    return enif_make_binary(env, &out);
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

    return norm(env, in, UNORM_NFC);
}

ERL_NIF_TERM to_nfd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return norm(env, in, UNORM_NFD);
}

ERL_NIF_TERM to_nfkc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return norm(env, in, UNORM_NFKC);
}

ERL_NIF_TERM to_nfkd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return norm(env, in, UNORM_NFKD);
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

    if (U_FAILURE(status)) {
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
    CHECK(env, status);
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
    CHECK(env, status);
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
    CHECK(env, status);
    return enif_make_binary(env, &out);

}
ERL_NIF_TERM len(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    cloner* ptr; 
    UBreakIterator* iter; 
    int count = 0, pos;
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

    pos = ubrk_first(iter);
    if (pos != UBRK_DONE)
        while (1) {
            pos = ubrk_next(iter);
            if (pos == UBRK_DONE)
                break;

            if (is_valid_elem(ptr, iter))
                count++;
        }

    return enif_make_int(env, count);
}

ERL_NIF_TERM split(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    cloner* ptr; 
    UBreakIterator* iter; 
    int len = -1, last, pos, is_valid;
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
        is_valid = is_valid_elem(ptr, iter);

        /* get the next elem. */
        pos = (int) ubrk_previous(iter);

        if (pos == UBRK_DONE)
            pos = 0;

        if (is_valid) /* Is the old element valid? */
        {
            len = FROM_ULEN(last - pos);

            bin = (UChar*) enif_make_new_binary(env, len, &head);
            memcpy(bin, 
                (const char*) (text + pos), 
                len);
            tail = enif_make_list_cell(env, head, tail);
        }
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
        if (is_valid_elem(ptr, iter))
        {
            head = enif_make_int(env, pos);
            tail = enif_make_list_cell(env, head, tail);
        }

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
    cloner_break* res_brk;

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
    iterType = TO_UBRK(type);

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

    res = (cloner*) enif_alloc_resource(iterator_type, 
                sizeof(cloner_break));
    res_brk = (cloner_break*) res;

    if (iterator_open(iter, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    switch (type) {
        case UBRK_WORD_ONLY:
            res_brk->skip_elem = &elem_word_none;
            break;
        default:
            res_brk->skip_elem = NULL;
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
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    iterator_type = enif_open_resource_type(env, NULL, "iterator_type",
        iterator_dtor, flags, NULL); 

    if (iterator_type == NULL) return 1;

    return 0;
}

#endif








