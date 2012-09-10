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


#include "i18n_nif.h"
#include "i18n_collation.h"

#if I18N_COLLATION
static UCollator* base_col;

static ErlNifEnv * global_collation_env;
static ERL_NIF_TERM available_locales;

/* Export for i18n_search module */
ErlNifResourceType* collator_type = 0;



/* Called from erl_nif. */
static void collator_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
static void collator_close(char* obj) 
{ 
    if (obj != NULL)
        ucol_close((UCollator*) obj);
}
static char* collator_clone(char* obj) 
{
    UErrorCode status = U_ZERO_ERROR;
    int32_t size = U_COL_SAFECLONE_BUFFERSIZE;

    obj = (char*) ucol_safeClone(
        (UCollator*) obj,
        NULL,
        &size,
        &status 
    );
    if(U_FAILURE(status)) { 
        return NULL;
    } 
    return obj;
}

static int collator_open(UCollator * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &collator_clone, &collator_close);
} 





static int parseAttrValue(const char * type) 
{
    return (!strcmp((char*) "primary", type))       ? UCOL_PRIMARY :
           (!strcmp((char*) "secondary", type))     ? UCOL_SECONDARY :
           (!strcmp((char*) "tertiary", type))      ? UCOL_TERTIARY :
           (!strcmp((char*) "quaternary", type))    ? UCOL_QUATERNARY :
           (!strcmp((char*) "identical", type))     ? UCOL_IDENTICAL :
           (!strcmp((char*) "default", type))       ? UCOL_DEFAULT  :

           (!strcmp((char*) "non_ignorable", type)) ? UCOL_NON_IGNORABLE :
           (!strcmp((char*) "shifted", type))       ? UCOL_SHIFTED :

           (!strcmp((char*) "on", type))            ? UCOL_ON :
           (!strcmp((char*) "off", type))           ? UCOL_OFF :

           (!strcmp((char*) "lower_first", type))   ? UCOL_LOWER_FIRST :
           (!strcmp((char*) "upper_first", type))   ? UCOL_UPPER_FIRST :
            -1;
}
static int parseAttrKey(const char * type) 
{
    return (!strcmp((char*) "alternate", type))      ? UCOL_ALTERNATE_HANDLING :
           (!strcmp((char*) "case_first", type))     ? UCOL_CASE_FIRST :
           (!strcmp((char*) "french_accents", type)) ? UCOL_FRENCH_COLLATION :
           (!strcmp((char*) "case_level", type))     ? UCOL_CASE_LEVEL  :
           (!strcmp((char*) "normalization", type))  ? UCOL_NORMALIZATION_MODE :
           (!strcmp((char*) "strength", type))       ? UCOL_STRENGTH :
           (!strcmp((char*) "hiragana", type))       ? 
                UCOL_HIRAGANA_QUATERNARY_MODE :
           (!strcmp((char*) "numeric", type))        ? UCOL_NUMERIC_COLLATION :
            -1;
}




/**
 * NIFs
 */

static int do_iterator_options(ErlNifEnv* env, UCollator* col, 
    const ERL_NIF_TERM in, unsigned int& i) {

    ERL_NIF_TERM out, list;
    ERL_NIF_TERM* tuple;
    unsigned int count;
    UErrorCode status = U_ZERO_ERROR;
    int32_t len; 

    char    value[ATOM_LEN], key[ATOM_LEN];
    int     parsed_value,    parsed_key;
    
    i = 0;
    if (!enif_get_list_length(env, in, &count)) 
        return 0;

    list = in;

    while (enif_get_list_cell(env, list, &out, &list)) {

        if (enif_get_tuple(env, out, &len, (const ERL_NIF_TERM**) &tuple)
            && (len == 2)) { 

            /* Set an attribute start */

            if (!(enif_get_atom(env, tuple[0], (char*) key,   
                        ATOM_LEN, ERL_NIF_LATIN1) 
               && enif_get_atom(env, tuple[1], (char*) value, 
                        ATOM_LEN, ERL_NIF_LATIN1))) 
                return 0;
                
    
            parsed_key   = parseAttrKey(key);
            parsed_value = parseAttrValue(value);
            if ((parsed_value == -1) || (parsed_key == -1)) 
                return 0;
 
            ucol_setAttribute(col,
                (UColAttribute)      parsed_key,
                (UColAttributeValue) parsed_value,
                &status);

            if (U_FAILURE(status))
                return 0;
            
            /* Set an attribute end */

        } else 
            return 0;
    }
    return 1;
}

/* Get a collator */
ERL_NIF_TERM get_collator(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out;
    char locale[LOCALE_LEN];
    UErrorCode status = U_ZERO_ERROR;
    UCollator* col;
    cloner* res;
    unsigned int index;


    if ((argc != 2) && (argc != 1))
        return enif_make_badarg(env);

    if (!enif_get_atom(env, argv[0], (char*) locale, 
            LOCALE_LEN, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    col = ucol_open((char *) locale, &status);
    CHECK(env, status);



    res = (cloner*) enif_alloc_resource(collator_type, sizeof(cloner));

    if (collator_open(col, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    CHECK_DEST(env, status,
        enif_release_resource(res);
    );


    if (argc == 2) {
        if (!do_iterator_options(env, col, argv[1], index)) {
            enif_release_resource(res);
            ERROR_ELEMENT(env, status, argv[1], index);
        }
    }

    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}


/* Get a rule-based collator */
ERL_NIF_TERM get_rule_collator(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ERL_NIF_TERM out;
    UErrorCode status = U_ZERO_ERROR;
    UParseError pe;
    UCollator* col;
    cloner* res;
    unsigned int index;



    if ((argc != 2) && (argc != 1))
        return enif_make_badarg(env);

    if(!enif_inspect_binary(env, argv[0], &in))
        return enif_make_badarg(env);

    col = ucol_openRules(
        (const UChar*) in.data, /* rules */
        TO_ULEN(in.size),
        UCOL_DEFAULT,
        UCOL_DEFAULT_STRENGTH,
        &pe,
        &status);
    if (U_FAILURE(status)) {
        ERROR_PARSE(env, status, &pe);
    }

    res = (cloner*) enif_alloc_resource(collator_type, sizeof(cloner));

    if (collator_open(col, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    CHECK_DEST(env, status,
        enif_release_resource(res);
    );


    if (argc == 2) {
        if (!do_iterator_options(env, col, argv[1], index)) {
            enif_release_resource(res);
            ERROR_ELEMENT(env, status, argv[1], index);
        }
    }

    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}

inline static void do_sort_key(
    ErlNifBinary  in,
    ErlNifBinary& out, 
    int32_t& len,
    UCollator* col,
    UErrorCode &status)
{
    int32_t full_len;
    status = U_ZERO_ERROR;

    if (!enif_alloc_binary(len, &out)) {
        status = U_MEMORY_ALLOCATION_ERROR;
        return;
    }

    full_len = ucol_getSortKey(col,
        (const UChar*) in.data, 
        TO_ULEN(in.size),
        (uint8_t*) out.data,  /* destination */
        len);

    /* If there was an internal error generating the sort key, 
        a zero value is returned. */
    if (!full_len) {
        enif_release_binary(&out);
        status = U_INTERNAL_PROGRAM_ERROR;
        return;
    }
    if (full_len > len) {
        status = U_BUFFER_OVERFLOW_ERROR;
        len = full_len;
        enif_release_binary(&out);
        return;
    }
    len = full_len;

    /* Delete NULL from the end of the key. */
    len--;

    if (len != (int32_t) out.size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(&out, len);
    }
}

ERL_NIF_TERM sort_key(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, out;
    int32_t len; 
    UCollator* col;
    cloner* ptr;
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 2)
        return enif_make_badarg(env);

    /* Second argument must be a binary */
    if(!(enif_inspect_binary(env, argv[1], &in)
      && enif_get_resource(env, argv[0], collator_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }
    col = (UCollator*) cloner_get(ptr);
    CHECK_RES(env, col);

    /* Convert a binary string in utf-8 to a binary string in utf-16. */
    len = in.size*4;
    do_sort_key(in, out, len, col, status);
    if (status == U_BUFFER_OVERFLOW_ERROR) {
        do_sort_key(in, out, len, col, status);
    }
    CHECK(env, status);
    return enif_make_binary(env, &out);
}

ERL_NIF_TERM compare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, in2;
    UCollator* col;
    cloner* ptr;
    UCollationResult res;
    ERL_NIF_TERM atom;

    if (argc != 3)
        return enif_make_badarg(env);

    if(!(enif_inspect_binary(env, argv[1], &in) 
      && enif_inspect_binary(env, argv[2], &in2)
      && enif_get_resource(env, argv[0], collator_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }
    col = (UCollator*) cloner_get(ptr);
    CHECK_RES(env, col);

    res = ucol_strcoll(col,
        (const UChar*) in.data, 
        TO_ULEN(in.size),
        (const UChar*) in2.data, 
        TO_ULEN(in2.size) 
    );

    switch (res) {
        case UCOL_EQUAL:
            atom = ATOM_EQUAL;
            break;
            
        case UCOL_GREATER: 
            atom = ATOM_GREATER;
            break;

        case UCOL_LESS:
            atom = ATOM_LESS;
            break;

        default:
            ERROR(env, U_INTERNAL_PROGRAM_ERROR);
    }

    return return_atom(env, atom);
}

ERL_NIF_TERM collator_locales(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return enif_make_copy(env, available_locales);
}









int i18n_collation_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    UErrorCode status = U_ZERO_ERROR;
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    base_col = ucol_open("", &status);
    if(U_FAILURE(status)) { 
        return 3;
    }
    collator_type = enif_open_resource_type(env, NULL, "collator_type",
        collator_dtor, flags, NULL); 
    if (collator_type == NULL) return 4;

    global_collation_env = enif_alloc_env();

    available_locales = generate_available(global_collation_env, 
        ucol_getAvailable, ucol_countAvailable());
    
    return 0;
}

void i18n_collation_unload(ErlNifEnv* /*env*/, void* /*priv*/)
{
    ucol_close(base_col);
    enif_free_env(global_collation_env);
    return;
}

#endif

