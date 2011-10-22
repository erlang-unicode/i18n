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
#include "i18n_string.h"
#include "i18n_collation.h"
#include "i18n_search.h"
#include "i18n_message.h"
#include "i18n_regex.h"
#include "i18n_locale.h"
#include "i18n_date.h"


ERL_NIF_TERM res_error_term;

ERL_NIF_TERM ATOM_TRUE, ATOM_FALSE;
ERL_NIF_TERM ATOM_EQUAL, ATOM_GREATER, ATOM_LESS;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ENDIAN;

ERL_NIF_TERM ATOM_COUNT;
ERL_NIF_TERM ATOM_RESOURCE;
ERL_NIF_TERM ATOM_SEARCH;





/* Define an interface for errors. */
ERL_NIF_TERM build_error(ErlNifEnv* env, ERL_NIF_TERM body) {
    return enif_make_tuple2(env, enif_make_atom(env, "i18n_error"), body);
}

ERL_NIF_TERM make_error(ErlNifEnv* env, const char* code) {
    return build_error(env,
        enif_make_atom(env, code));
}

ERL_NIF_TERM parse_error(ErlNifEnv* env, UErrorCode status, 
        UParseError* e) {
    return build_error(env, 
        enif_make_tuple3(env,
            enif_make_atom(env, u_errorName(status)),
            enif_make_tuple2(env,
                enif_make_atom(env, "line"),
                enif_make_int(env, (int) e->line)),
            enif_make_tuple2(env,
                enif_make_atom(env, "offset"),
                enif_make_int(env, (int) e->offset))
            ));
}

ERL_NIF_TERM list_element_error(ErlNifEnv* env, 
    const ERL_NIF_TERM list, int32_t num) {
    return build_error(env, 
        enif_make_tuple3(env,
            enif_make_atom(env, "bad_element"),
            enif_make_tuple2(env,
                enif_make_atom(env, "list"),
                list),
            enif_make_tuple2(env,
                enif_make_atom(env, "index"),
                enif_make_int(env, (int) num))
            ));
}


int i18n_atom_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    ATOM_TRUE     = enif_make_atom(env, "true");
    ATOM_FALSE    = enif_make_atom(env, "false");

    ATOM_EQUAL    = enif_make_atom(env, "equal");
    ATOM_GREATER  = enif_make_atom(env, "greater");
    ATOM_LESS     = enif_make_atom(env, "less");

    ATOM_OK       = enif_make_atom(env, "ok");

    ATOM_COUNT    = enif_make_atom(env, "count");
    ATOM_RESOURCE = enif_make_atom(env, "resource");
    ATOM_SEARCH   = enif_make_atom(env, "search");

#if U_IS_BIG_ENDIAN
    ATOM_ENDIAN = enif_make_atom(env, "big");
#else
    ATOM_ENDIAN = enif_make_atom(env, "little");
#endif

    res_error_term = make_error(env, "resource_error");

    return 0;
}






ERL_NIF_TERM enum_to_term(ErlNifEnv* env, UEnumeration* en) {
    
    ERL_NIF_TERM head, tail;
    UErrorCode status = U_ZERO_ERROR;
    const char* buf;
    int32_t len;


    uenum_reset(en, &status);   
    CHECK(env, status);

    tail = enif_make_list(env, 0);

    while (true) {
        buf = uenum_next(en, &len, &status);   
        CHECK(env, status);
        if (buf == NULL) 
            return tail;

        if (len > 255) 
            return make_error(env, "too_long_enum_element");

        head = enif_make_atom(env, buf);
        tail = enif_make_list_cell(env, head, tail);
    }
}







ERL_NIF_TERM generate_available(ErlNifEnv* env, avail_fun fun, 
    int32_t i)
{
    ERL_NIF_TERM head, tail;
    const char* locale;

    tail = enif_make_list(env, 0);
    while (i) {
        i--;
        locale = fun(i);
        head = enif_make_atom(env, locale);
        tail = enif_make_list_cell(env, head, tail);
    }

    return tail;
}











































































































































#if I18N_TRANS
static ErlNifResourceType* trans_type = 0;



/* Called from erl_nif. */
void trans_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
void trans_close(char* obj) 
{ 
    if (obj != NULL)
        utrans_close((UTransliterator*) obj);
}
char* trans_clone(char* obj) 
{
    UErrorCode status = U_ZERO_ERROR;

    obj = (char*) utrans_clone(
        (const UTransliterator *) obj,
        &status 
    );
    if(U_FAILURE(status)) { 
        return NULL;
    } 
    return obj;
}

int trans_open(UTransliterator * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &trans_clone, &trans_close);
} 


int parseDir(const char * type) 
{
    return (!strcmp((char*) "reverse", type)) ? UTRANS_REVERSE :
        UTRANS_FORWARD;
}


static ERL_NIF_TERM trans_ids(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    ERL_NIF_TERM out;
    UEnumeration* en; 
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 0)
        return enif_make_badarg(env);

    en = utrans_openIDs(&status);   
    CHECK(env, status);

    out = enum_to_term(env, en);
    uenum_close(en);

    return out;
}


/* Get a transliterator */
static ERL_NIF_TERM get_transliterator(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out;
    char id[LOCALE_LEN], dir[ATOM_LEN];
    UErrorCode status = U_ZERO_ERROR;
    UTransliterator* obj;
    cloner* res;
    int parsed_dir;


    if (argc != 2)
        return enif_make_badarg(env);

    if (!(enif_get_atom(env, argv[0], (char*) id, LOCALE_LEN, ERL_NIF_LATIN1)
       && enif_get_atom(env, argv[1], (char*) dir, ATOM_LEN, ERL_NIF_LATIN1)
        )) {
        return enif_make_badarg(env);
    }

    parsed_dir = parseDir(dir);
    if ((parsed_dir == -1)) 
        return enif_make_badarg(env);

    obj = utrans_open((char *) id, (UTransDirection) parsed_dir, 
            NULL, 0, NULL, &status);
    CHECK(env, status);



    res = (cloner*) enif_alloc_resource(trans_type, sizeof(cloner));

    if (trans_open(obj, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    CHECK_DEST(env, status,
        enif_release_resource(res);
    );


    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}


static ERL_NIF_TERM trans(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    cloner* ptr; 
    const Transliterator* t; 
    UnicodeString input;

    if (argc != 2)
        return enif_make_badarg(env);

    /* Second argument must be a binary */
    if(!(enif_inspect_binary(env, argv[1], &in)
      && enif_get_resource(env, argv[0], trans_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }

    t = (Transliterator*) cloner_get(ptr);
    CHECK_RES(env, t);
            

    input = copy_binary_to_string(in);

    t->transliterate(input);
    
    return string_to_term(env, input);
}

static int i18n_trans_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    trans_type = enif_open_resource_type(env, NULL, "trans_type",
        trans_dtor, flags, NULL); 
    if (collator_type == NULL) return 20;
    
    return 0;
}
#endif












static ERL_NIF_TERM i18n_info(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

#if I18N_INFO
    ERL_NIF_TERM head, tail;
    tail = enif_make_list(env, 0);

    #if I18N_SEARCH
        head = enif_make_tuple2(env, ATOM_SEARCH, i18n_search_info(env));
        tail = enif_make_list_cell(env, head, tail);
    #endif

    return tail;
#else
    return enif_make_list(env, 0);
#endif
}









static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    static int is_loaded = 0;
    int code;

    if (is_loaded)
        return 0;

    uloc_getDefault();

    code = i18n_atom_load(env, priv_data, load_info);
    if (code) return code;

#if I18N_STRING
    code = i18n_string_load(env, priv_data, load_info);
    if (code) return code;
#endif


#if I18N_COLLATION
    code = i18n_collation_load(env, priv_data, load_info);
    if (code) return code;
#endif


#if I18N_COLLATION
    code = i18n_search_load(env, priv_data, load_info);
    if (code) return code;
#endif


#if I18N_MESSAGE
    code = i18n_message_load(env, priv_data, load_info);
    if (code) return code;
#endif


#if I18N_REGEX
    code = i18n_regex_load(env, priv_data, load_info);
    if (code) return code;
#endif


#if I18N_DATE
    code = i18n_date_load(env, priv_data, load_info);
    if (code) return code;
#endif


#if I18N_TRANS
    code = i18n_trans_load(env, priv_data, load_info);
    if (code) return code;
#endif
     
    is_loaded = 1;
     
    return 0;
}


static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return load(env, priv, load_info);
}

static int upgrade(ErlNifEnv* /*env*/, void** /*priv*/, void** /*old_priv*/,
          ERL_NIF_TERM /*load_info*/)
{
    return 0;
}

static void unload(ErlNifEnv* env, void* priv)
{
#if I18N_COLLATION
    i18n_collation_unload(env, priv);
#endif
    return;
}










static ErlNifFunc nif_funcs[] =
{
    {"i18n_info",    0, i18n_info},

#if I18N_STRING
    {"to_utf8",      1, to_utf8},
    {"from_utf8",    1, from_utf8},
    {"endian",       0, endian},

    /* Locale dependible */
    {"get_iterator", 2, get_iterator},
    {"len",          2, len},
    {"split",        2, split},
    {"split_index",  2, split_index},
    {"to_lower",     2, to_lower},
    {"to_upper",     2, to_upper},
    {"to_title",     2, to_title},
    {"to_nfc",       1, to_nfc},
    {"to_nfd",       1, to_nfd},
    {"to_nfkc",      1, to_nfkc},
    {"to_nfkd",      1, to_nfkd},

    {"iterator_locales",    0, iterator_locales},
#endif





#if I18N_COLLATION
    {"get_collator",      1, get_collator},
    {"get_collator",      2, get_collator},
    {"sort_key",          2, sort_key},
    {"compare",           3, compare},

    {"collator_locales",    0, collator_locales},
#endif



#if I18N_SEARCH
    {"search_open",       2, search_open},
    {"search_index",      2, search_index},
    {"search_match_all",  2, search_match_all},
    {"search_match",      2, search_match},
    {"search_test",       2, search_test},
#endif





#if I18N_MESSAGE
    {"open_format", 2, open_format},
    {"format",      2, format},
    {"format",      3, format},
#endif




#if I18N_REGEX
    {"open_regex",        1, open_regex},
    {"regex_replace",     3, regex_replace},
    {"regex_replace_all", 3, regex_replace_all},
    {"regex_split",       2, regex_split},
    {"regex_test",        2, regex_test}, /* hello eunit */
    {"regex_match",       2, regex_match}, 
    {"regex_match_all",   2, regex_match_all}, 
#endif




#if I18N_LOCALE
    {"locale_name",         1, locale_name},
    {"locale_parent",       1, locale_parent},
    {"locale_language_tag", 1, locale_language_tag},
    {"locale_base_name",    1, locale_base_name},
#endif




#if I18N_DATE
    {"date_now",         0, date_now},
    {"open_calendar",    1, open_calendar},
    {"open_calendar",    2, open_calendar},
    {"open_calendar",    3, open_calendar},
    {"date_set",         3, date_set},
    {"date_add",         3, date_add},
    {"date_roll",        3, date_roll},
    {"date_clear",       3, date_clear},
    {"date_is_weekend",  2, date_is_weekend},
    {"date_get",         4, date_get3},
    {"date_get",         7, date_get6},
    {"date_get_field",   3, date_get_field},
    {"date_get_fields",  3, date_get_fields},

    {"calendar_locales",    0, calendar_locales},
#endif




#if I18N_TRANS
    {"trans_ids",          0, trans_ids},
    {"trans",              2, trans},
    {"get_transliterator", 2, get_transliterator},
#endif

};
ERL_NIF_INIT(i18n_nif,nif_funcs,load,reload,upgrade,unload)


