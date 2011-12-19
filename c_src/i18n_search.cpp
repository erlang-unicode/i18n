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
#include "i18n_search.h"


#if I18N_SEARCH

/* isearch_common_store: Stores const values for each copy of searcher */
static ErlNifResourceType* isearch_common_type = 0;

#if I18N_INFO
static int isearch_count = 0;
static ErlNifMutex* isearch_count_mtx;
static int isearch_common_count = 0;
static ErlNifMutex* isearch_common_count_mtx;
#endif

/* Stores default text */
static UChar isearch_text[1];
static int32_t isearch_text_len = 1;

typedef struct {
    ErlNifEnv* env;
    ERL_NIF_TERM pattern;
    ERL_NIF_TERM col;
} ISearchCommon;


static inline ISearchCommon* isearch_common_alloc() 
{
#if I18N_INFO
    enif_mutex_lock(isearch_common_count_mtx);
    isearch_common_count++;
    enif_mutex_unlock(isearch_common_count_mtx);
#endif

    return (ISearchCommon*) enif_alloc_resource(isearch_common_type, 
                sizeof(ISearchCommon));
}

static void isearch_common_dtor(ErlNifEnv* /*env*/, void* obj) 
{
#if I18N_INFO
    enif_mutex_lock(isearch_common_count_mtx);
    isearch_common_count--;
    enif_mutex_unlock(isearch_common_count_mtx);
#endif

    enif_free_env(((ISearchCommon*) obj)->env);
}


static ISearchCommon* isearch_common_open(const ERL_NIF_TERM& pattern, 
                                   const ERL_NIF_TERM& col)
{
    ISearchCommon* isc;
    isc = isearch_common_alloc();

    isc->env = enif_alloc_env();
    isc->pattern = enif_make_copy(isc->env, pattern);
    isc->col     = enif_make_copy(isc->env, col);
    
    return isc;
}









/* ISearch API */
typedef struct {
    ISearchCommon* isc;
    UStringSearch* ss;
} ISearch;

/* @private */
/**
 * Convert ISearchCommon to UStringSearch
 */
static UStringSearch* isc_to_uss(ISearchCommon* isc)
{
    ErlNifBinary pattern;
    cloner* ptr;
    const UCollator* col;
    UStringSearch* ss;
    UBreakIterator* bi = NULL;
    UErrorCode status = U_ZERO_ERROR;


    if(!(enif_get_resource(isc->env, isc->col, collator_type, (void**) &ptr)
      && enif_inspect_binary(isc->env, isc->pattern, &pattern))) {
        return NULL;
    }
    col = (UCollator*) cloner_get(ptr);
    if (col == NULL)
        return NULL;

    /* Init with "empty" text. */
    ss = usearch_openFromCollator(
        (const UChar *) pattern.data,
        (int32_t) TO_ULEN(pattern.size),
        (const UChar *) isearch_text,
        isearch_text_len,
        col,
        bi,
        &status);
    if(U_FAILURE(status)) { 
        return NULL;
    } 

    return ss;
}

static void isearch_close(ISearch* is)
{
    usearch_close(is->ss);
    /* important: free the common resourse for this clone */
    enif_release_resource(is->isc);
    enif_free(is);
}

static ISearch* isearch_clone(ISearch* is1)
{
    ISearch* is2;

    if (is1 == NULL)
        return NULL;

    is2 = (ISearch*) enif_alloc(sizeof(ISearch));
    if (is2 == NULL)
        return NULL;

    is2->isc = is1->isc;
    /* important: keep the common resourse for new clone */
    enif_keep_resource(is2->isc);

    is2->ss = isc_to_uss(is2->isc);
    if (is2->ss == NULL) {
        isearch_close(is2);
    }
    return is2;
}

static ISearch* isearch_open(const ERL_NIF_TERM& col,
                      const ERL_NIF_TERM& pattern)
{
    ISearch* is;
    is =(ISearch*) enif_alloc(sizeof(ISearch));
    if (is == NULL)
        return NULL;

    is->isc = isearch_common_open(pattern, col);
    is->ss = isc_to_uss(is->isc);
    if (is->ss == NULL) {
        isearch_close(is);
    }
    return is;
}

static UStringSearch* isearch_get(const ISearch* is)
{
    if ((is == NULL) || (is->ss == NULL))
        return NULL;
    return is->ss;
}









/* This constraction is same for all parts of this file. */
static ErlNifResourceType* searcher_type = 0;



/* Called from erl_nif. */
static void searcher_dtor(ErlNifEnv* /*env*/, void* obj) 
{
#if I18N_INFO
    enif_mutex_lock(isearch_count_mtx);
    isearch_count--;
    enif_mutex_unlock(isearch_count_mtx);
#endif

    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
static void searcher_close(char* obj) 
{ 
    if (obj != NULL)
        isearch_close((ISearch*) obj);
}
static char* searcher_clone(char* obj) 
{
    return (char*) isearch_clone((ISearch*) obj);
}

static int searcher_open(ISearch * obj, cloner* c)
{
#if I18N_INFO
    enif_mutex_lock(isearch_count_mtx);
    isearch_count++;
    enif_mutex_unlock(isearch_count_mtx);
#endif

    return cloner_open((char *) obj, c, &searcher_clone, &searcher_close);
} 





/**
 * User API
 */


ERL_NIF_TERM search_open(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ISearch* is;
    cloner* res;
    ERL_NIF_TERM out;

    if (argc != 2)
        return enif_make_badarg(env);

    /* argv[0] = col, argv[1] = pattern */
    is = isearch_open(argv[0], argv[1]);
    
    
    res = (cloner*) enif_alloc_resource(searcher_type, sizeof(cloner));
    if (searcher_open(is, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}

inline UStringSearch* search_read_args(ErlNifEnv* env, 
        const ERL_NIF_TERM argv[], UErrorCode& status)
{
    ErlNifBinary text;
    cloner* ptr;
    const ISearch* is;
    UStringSearch* ss;

    /* Second argument must be a binary */
    if(!(enif_get_resource(env, argv[0], searcher_type, (void**) &ptr)
      && enif_inspect_binary(env, argv[1], &text))) 
        return NULL;

    is = (const ISearch*) cloner_get(ptr);
    if (is == NULL)
        return NULL;

    ss = isearch_get(is);
    if (ss == NULL)
        return NULL;

    usearch_reset(ss);

    usearch_setText(ss,
        (const UChar *) text.data,
        (int32_t) TO_ULEN(text.size),
        &status);
    if (U_FAILURE(status)) 
        return NULL;

    return ss;

}

ERL_NIF_TERM search_index(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UStringSearch* ss;
    UErrorCode status = U_ZERO_ERROR;
    ERL_NIF_TERM head, tail;
    int pos; 

    if (argc != 2)
        return enif_make_badarg(env);

    ss = search_read_args(env, argv, status);
    CHECK(env, status);
    CHECK_RES(env, ss);

    pos = (int) usearch_last(ss, &status);
    CHECK(env, status);

    tail = enif_make_list(env, 0);
    while (pos != USEARCH_DONE) 
    {
        head = enif_make_tuple2(env, 
            enif_make_int(env, pos),
            enif_make_int(env, (int) usearch_getMatchedLength(ss))
        );
        tail = enif_make_list_cell(env, head, tail);

        /* get the next elem. */
        pos = (int) usearch_previous(ss, &status);
        CHECK(env, status);
    }

    return tail;
}
ERL_NIF_TERM search_match_all(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UStringSearch* ss;
    UErrorCode status = U_ZERO_ERROR;
    ERL_NIF_TERM head, tail;
    int pos, len; 
    UChar* bin;
    const UChar* text;
    int32_t text_len;

    if (argc != 2)
        return enif_make_badarg(env);

    ss = search_read_args(env, argv, status);
    CHECK(env, status);
    CHECK_RES(env, ss);


    pos = (int) usearch_last(ss, &status);
    CHECK(env, status);
    text = usearch_getText(ss, &text_len);

    tail = enif_make_list(env, 0);
    while (pos != USEARCH_DONE) 
    {
        len = FROM_ULEN(usearch_getMatchedLength(ss));

        bin = (UChar*) enif_make_new_binary(env, len, &head);
        memcpy(bin, 
            (const char*) (text + pos), 
            len);
        tail = enif_make_list_cell(env, head, tail);



        /* get the next elem. */
        pos = (int) usearch_previous(ss, &status);
        CHECK(env, status);
    }

    return tail;
}
ERL_NIF_TERM search_match(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UStringSearch* ss;
    UErrorCode status = U_ZERO_ERROR;
    ERL_NIF_TERM res;
    int pos, len; 
    UChar* bin;
    const UChar* text;
    int32_t text_len;

    if (argc != 2)
        return enif_make_badarg(env);

    ss = search_read_args(env, argv, status);
    CHECK(env, status);
    CHECK_RES(env, ss);


    pos = (int) usearch_last(ss, &status);
    CHECK(env, status);

    if (pos != USEARCH_DONE) 
    {
        len = FROM_ULEN(usearch_getMatchedLength(ss));

        bin = (UChar*) enif_make_new_binary(env, len, &res);
        text = usearch_getText(ss, &text_len);
        memcpy(bin, 
            (const char*) (text + pos), 
            len);
    } else {
        res = return_atom(env, ATOM_FALSE);
    }

    return res;
}
ERL_NIF_TERM search_test(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UStringSearch* ss;
    UErrorCode status = U_ZERO_ERROR;
    int pos; 

    if (argc != 2)
        return enif_make_badarg(env);

    ss = search_read_args(env, argv, status);
    CHECK(env, status);
    CHECK_RES(env, ss);


    pos = (int) usearch_last(ss, &status);
    CHECK(env, status);

    return bool_to_term(env, pos != USEARCH_DONE);
}

#if I18N_INFO
ERL_NIF_TERM i18n_search_info(ErlNifEnv *env)
{
    return  enif_make_list1(env, 
                enif_make_tuple2(env,
                    return_atom(env, ATOM_RESOURCE),
                    enif_make_list2(env,
                        enif_make_tuple2(env,
                            enif_make_atom(env, (const char*) "isearch"),
                                enif_make_list1(env, 
                                    enif_make_tuple2(env,
                                        return_atom(env, ATOM_COUNT),
                                        enif_make_int(env, isearch_count)))),
                        enif_make_tuple2(env,
                            enif_make_atom(env, (const char*) "isearch_common"),
                                enif_make_list1(env, 
                                    enif_make_tuple2(env,
                                        return_atom(env, ATOM_COUNT),
                                        enif_make_int(env,
                                            isearch_common_count))))
            )));
}
#endif

int i18n_search_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

#if I18N_INFO
    isearch_count_mtx = 
        enif_mutex_create((char*) "isearch_count_mtx");
    isearch_common_count_mtx = 
        enif_mutex_create((char*) "isearch_common_count_mtx");
#endif

    isearch_common_type = enif_open_resource_type(env, NULL, 
        "isearch_common_type", isearch_common_dtor, flags, NULL); 
    if (isearch_common_type == NULL) return 40;

    searcher_type = enif_open_resource_type(env, NULL, "searcher_type",
        searcher_dtor, flags, NULL); 
    if (searcher_type == NULL) return 41;
    return 0;
}

#endif

