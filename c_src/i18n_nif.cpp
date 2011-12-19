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
#include "i18n_trans.h"
#include "unicode/uversion.h"
#include "unicode/uchar.h"


static ErlNifEnv * global_atom_env;

ERL_NIF_TERM res_error_term;

ERL_NIF_TERM ATOM_TRUE, ATOM_FALSE;
ERL_NIF_TERM ATOM_EQUAL, ATOM_GREATER, ATOM_LESS;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ENDIAN;

ERL_NIF_TERM ATOM_COUNT;
ERL_NIF_TERM ATOM_RESOURCE;
ERL_NIF_TERM ATOM_SEARCH;

ERL_NIF_TERM ATOM_ICU_VERSION, ATOM_UNICODE_VERSION;



/**
 * Reverse an Erlang list.
 */
ERL_NIF_TERM reverse_list(ErlNifEnv *env, ERL_NIF_TERM tail) {
    ERL_NIF_TERM out = enif_make_list(env, 0);
    ERL_NIF_TERM head;

    while(enif_get_list_cell(env, tail, &head, &tail))
        out = enif_make_list_cell(env, head, out);
    
    return out;
}




/* Define an interface for errors. */
ERL_NIF_TERM build_error(ErlNifEnv* env, ERL_NIF_TERM body) {
    return enif_make_tuple2(env, enif_make_atom(env, "i18n_error"), body);
}

/**
 * Pass an error to Erlang code.
 * Error as a string will be converted to an atom.
 */
ERL_NIF_TERM make_error(ErlNifEnv* env, const char* code) {
    return build_error(env,
        enif_make_atom(env, code));
}

/**
 * Convert an UErrorCode to an atom. 
 */
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

/**
 * An element it the list has a bad type.
 * Used by i18n_message. 
 */
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


/**
 * Alloc atoms
 */
int i18n_atom_load(ErlNifEnv* /*env*/, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    global_atom_env = enif_alloc_env();

    ATOM_TRUE     = enif_make_atom(global_atom_env, "true");
    ATOM_FALSE    = enif_make_atom(global_atom_env, "false");

    ATOM_EQUAL    = enif_make_atom(global_atom_env, "equal");
    ATOM_GREATER  = enif_make_atom(global_atom_env, "greater");
    ATOM_LESS     = enif_make_atom(global_atom_env, "less");

    ATOM_OK       = enif_make_atom(global_atom_env, "ok");

    ATOM_COUNT    = enif_make_atom(global_atom_env, "count");
    ATOM_RESOURCE = enif_make_atom(global_atom_env, "resource");
    ATOM_SEARCH   = enif_make_atom(global_atom_env, "search");

#if U_IS_BIG_ENDIAN
    ATOM_ENDIAN = enif_make_atom(global_atom_env, "big");
#else
    ATOM_ENDIAN = enif_make_atom(global_atom_env, "little");
#endif

    ATOM_ICU_VERSION = enif_make_atom(global_atom_env, U_ICU_VERSION);
    ATOM_UNICODE_VERSION = enif_make_atom(global_atom_env, U_UNICODE_VERSION);

    res_error_term = make_error(global_atom_env, "resource_error");

    return 0;
}

void i18n_atom_unload(ErlNifEnv* /*env*/, void* /*priv*/)
{
    enif_free_env(global_atom_env);
    return;
}





/**
 * Convert an ICU enum to a list of atoms.
 * Used by some C++ code, when we need to extract the list of locales.
 * Used by i18n_trans.
 */
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


ERL_NIF_TERM enum_to_term(ErlNifEnv* env, StringEnumeration* en) {
    
    ERL_NIF_TERM head, tail;
    UErrorCode status = U_ZERO_ERROR;
    const char* buf;
    int32_t len;


    en->reset(status);   
    CHECK(env, status);

    tail = enif_make_list(env, 0);

    while (true) {
        buf = en->next(&len, status);
        CHECK(env, status);
        if (buf == NULL) 
            return tail;

        if (len > 255) 
            return make_error(env, "too_long_enum_element");

        head = enif_make_atom(env, buf);
        tail = enif_make_list_cell(env, head, tail);
    }
}






/**
 * Generate a list if locales.
 */
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












static ERL_NIF_TERM i18n_info(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{

#if I18N_INFO
    ERL_NIF_TERM head, tail;
    tail = enif_make_list(env, 0);

    #if I18N_SEARCH
        head = enif_make_tuple2(env, return_atom(env, ATOM_SEARCH), 
                i18n_search_info(env));
        tail = enif_make_list_cell(env, head, tail);
    #endif

    return tail;
#else
    return enif_make_list(env, 0);
#endif
}



static ERL_NIF_TERM icu_version(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{
    return return_atom(env, ATOM_ICU_VERSION);
}

static ERL_NIF_TERM unicode_version(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{
    return return_atom(env, ATOM_UNICODE_VERSION);
}


#if I18N_TEST
static ERL_NIF_TERM test_error(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{
    CHECK(env, U_PARSE_ERROR);

    return return_atom(env, ATOM_TRUE);
}

static ERL_NIF_TERM test_list_element_error(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{
    ERL_NIF_TERM list;

    list = enif_make_list(env, 0);
    return list_element_error(env, list, 1);
}

static ERL_NIF_TERM test_parse_error(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{
    UParseError e;
    e.line = 0;
    e.offset = 0;

    return parse_error(env, U_PARSE_ERROR, &e);
}

static ERL_NIF_TERM test_make_error(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{
    return make_error(env, "it_is_a_test_error");
}
#endif






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
#if I18N_STRING
    i18n_string_unload(env, priv);
#endif

#if I18N_COLLATION
    i18n_collation_unload(env, priv);
#endif

#if I18N_DATE
    i18n_date_unload(env, priv);
#endif

#if I18N_TRANS
    i18n_trans_unload(env, priv);
#endif

    i18n_atom_unload(env, priv);

    return;
}









/**
 * Define the private API.
 */

static ErlNifFunc nif_funcs[] =
{
    {"i18n_info",    0, i18n_info},
    {"icu_version",  0, icu_version},
    {"unicode_version", 0, unicode_version},

#if I18N_TEST
    /* Tests */
    {"test_error",              0, test_error},
    {"test_parse_error",        0, test_parse_error},
    {"test_list_element_error", 0, test_list_element_error},
    {"test_make_error",         0, test_make_error},
#endif

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
    {"case_compare", 3, case_compare},
    {"non_case_compare", 2, non_case_compare},

    {"iterator_locales",    0, iterator_locales},
#endif





#if I18N_COLLATION
    {"get_collator",      1, get_collator},
    {"get_collator",      2, get_collator},
    {"get_rule_collator", 1, get_rule_collator},
    {"get_rule_collator", 2, get_rule_collator},
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
    {"date_get",         4, date_get3},
    {"date_get",         7, date_get6},
    {"date_get_field",   3, date_get_field},
    {"date_get_fields",  3, date_get_fields},
    {"date_diff_field",  4, date_diff_field},
    {"date_diff_fields", 4, date_diff_fields},

    {"calendar_locales", 0, calendar_locales},
    {"timezone_ids",     0, timezone_ids},
#endif




#if I18N_TRANS
    {"trans_ids",          0, trans_ids},
    {"trans",              2, trans},
    {"get_transliterator", 2, get_transliterator},
#endif

};

/* Pass information to VM */
ERL_NIF_INIT(i18n_nif,nif_funcs,load,reload,upgrade,unload)


