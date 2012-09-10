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
#include "i18n_trans.h"


#if I18N_TRANS
static ErlNifResourceType* trans_type = 0;

static ErlNifEnv * global_trans_env;
static ERL_NIF_TERM available_ids;



/* Called from erl_nif. */
static void trans_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
static void trans_close(char* obj) 
{ 
    if (obj != NULL)
        utrans_close((UTransliterator*) obj);
}
static char* trans_clone(char* obj) 
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

static int trans_open(UTransliterator * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &trans_clone, &trans_close);
} 


static int parseDir(const char * type) 
{
    return (!strcmp((char*) "reverse", type)) ? UTRANS_REVERSE :
        UTRANS_FORWARD;
}


static ERL_NIF_TERM 
get_trans_ids(ErlNifEnv* env, UErrorCode& status)
{
    ERL_NIF_TERM out;
    UEnumeration* en;

    en = utrans_openIDs(&status);
    CHECK(env, status);

    out = enum_to_term(env, en);
    uenum_close(en);

    return out;
}

ERL_NIF_TERM trans_ids(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return enif_make_copy(env, available_ids);
}


/* Get a transliterator */
ERL_NIF_TERM get_transliterator(ErlNifEnv* env, int argc, 
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

    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}


ERL_NIF_TERM trans(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

int i18n_trans_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    UErrorCode status = U_ZERO_ERROR;

    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    trans_type = enif_open_resource_type(env, NULL, "trans_type",
        trans_dtor, flags, NULL); 

    global_trans_env = enif_alloc_env();

    available_ids = get_trans_ids(env, status);
    if (U_FAILURE(status))
        return 1000;
    available_ids = reverse_list(env, available_ids);
    available_ids = enif_make_copy(global_trans_env, available_ids);
    
    return 0;
}

void i18n_trans_unload(ErlNifEnv* /*env*/, void* /*priv*/)
{
    enif_free_env(global_trans_env);
    return;
}
#endif
