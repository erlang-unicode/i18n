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
#include "i18n_regex.h"



#if I18N_REGEX

static ErlNifResourceType* regex_type = 0;


/* Called from erl_nif. */
static void regex_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
static void regex_close(char* obj) 
{ 
    if (obj != NULL)
        delete (RegexPattern*) obj;
}
static char* regex_clone(char* obj) 
{
    obj = (char*) new RegexPattern(
        * ((RegexPattern*) obj)
    );
    return obj;
}

static int regex_open(RegexPattern * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &regex_clone, &regex_close);
} 








/**
 * NIFs
 */

/* Get a message format  */
ERL_NIF_TERM open_regex(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out;
    ErlNifBinary in;
    UParseError pe;
    UErrorCode status = U_ZERO_ERROR;
    RegexPattern* re;
    cloner* res;
    uint32_t flags = 0;
    UnicodeString input;

    if (argc != 1)
        return enif_make_badarg(env);

    if(!(enif_inspect_binary(env, argv[0], &in)  /* Regexp */
        )) {
        return enif_make_badarg(env);
    }
    input = binary_to_string(in);

    re = RegexPattern::compile(
            input, 
            flags, 
            pe, 
            status);
    if (U_FAILURE(status)) {
        ERROR_PARSE(env, status, &pe);
    }

    res = (cloner*) enif_alloc_resource(regex_type, sizeof(cloner));
    if (regex_open(re, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}


/* i18n_regex:replace(i18n_regex:open(i18n_string:from("G")),
   i18n_string:from("$1"), i18n_string:from("G")). */
ERL_NIF_TERM regex_replace_all(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, in2;
    RegexPattern* re;
    RegexMatcher* rm;
    cloner* ptr;
    UnicodeString input, replacement, res;
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 3)
        return enif_make_badarg(env);

    if(!(enif_inspect_binary(env, argv[2], &in)  /* Subject */
      && enif_inspect_binary(env, argv[1], &in2) /* RE */
      && enif_get_resource(env, argv[0], regex_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }

    re = (RegexPattern*) cloner_get(ptr);
    CHECK_RES(env, re);

    input = binary_to_string(in);
    rm = re->matcher((const UnicodeString) input, status);
    CHECK(env, status);

    replacement = binary_to_string(in2);
    res = rm->replaceAll(
        (const UnicodeString) replacement,
        status 
    );
    delete rm;
    CHECK(env, status);

    return string_to_term(env, res);
}

ERL_NIF_TERM regex_replace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, in2;
    RegexPattern* re;
    RegexMatcher* rm;
    cloner* ptr;
    UnicodeString input, replacement, res;
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 3)
        return enif_make_badarg(env);

    if(!(enif_inspect_binary(env, argv[2], &in)  /* Subject */
      && enif_inspect_binary(env, argv[1], &in2) /* RE */
      && enif_get_resource(env, argv[0], regex_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }

    re = (RegexPattern*) cloner_get(ptr);
    CHECK_RES(env, re);

    input = binary_to_string(in);
    rm = re->matcher((const UnicodeString) input, status);
    CHECK(env, status);

    replacement = binary_to_string(in2);
    res = rm->replaceFirst(
        (const UnicodeString) replacement,
        status 
    );
    delete rm;
    CHECK(env, status);

    return string_to_term(env, res);
}



/* i18n_regex:split(i18n_regex:open(i18n_string:from("G")),
       i18n_string:from("4")). */
#define BUF_CNT 20
ERL_NIF_TERM regex_split(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ERL_NIF_TERM head, tail;
    int32_t num; 
    RegexPattern* re;
    cloner* ptr;
    UErrorCode status = U_ZERO_ERROR;
    UnicodeString fields[BUF_CNT];
    UnicodeString input;

    if (argc != 2)
        return enif_make_badarg(env);

    if(!(enif_inspect_binary(env, argv[1], &in)
      && enif_get_resource(env, argv[0], regex_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }

    re = (RegexPattern*) cloner_get(ptr);
    CHECK_RES(env, re);

    input = binary_to_string(in);

    num = re->split(
        (const UnicodeString) input,
        fields,
        (int32_t) BUF_CNT,
        status 
    );   
    CHECK(env, status);

    tail = enif_make_list(env, 0);
    while(num>0) {
        num--;

        head = string_to_term(env, fields[num]);
        tail = enif_make_list_cell(env, head, tail);
    }

    return tail;
}

ERL_NIF_TERM regex_test(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    RegexPattern* re;
    RegexMatcher* rm;
    cloner* ptr;
    UnicodeString input;
    UErrorCode status = U_ZERO_ERROR;
    UBool res;

    if (argc != 2)
        return enif_make_badarg(env);

    /* Second argument must be a binary  */
    if(!(enif_inspect_binary(env, argv[1], &in)  /* Subject */
      && enif_get_resource(env, argv[0], regex_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }

    re = (RegexPattern*) cloner_get(ptr);
    CHECK_RES(env, re);

    input = binary_to_string(in);
    rm = re->matcher((const UnicodeString) input, status);
    CHECK(env, status);

    res = rm->matches(
        status 
    );
    delete rm;
    CHECK(env, status);

    return bool_to_term(env, res);
}


inline static ERL_NIF_TERM do_regex_match(ErlNifEnv* env, 
    RegexMatcher* rm, UErrorCode& status)
{
    ERL_NIF_TERM head, tail;
    UnicodeString group;
    int32_t num;

    num = rm->groupCount();
    tail = enif_make_list(env, 0);

    while(num >= 0) {

        /* Replace the temp string */
        group = rm->group(num, status);
        CHECK(env, status);

        head = string_to_term(env, group);
        tail = enif_make_list_cell(env, head, tail);
        num--;
    }
    return tail;
}

/* Returns a reversed variant. */

ERL_NIF_TERM regex_match_all(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ERL_NIF_TERM head, tail;
    RegexPattern* re;
    RegexMatcher* rm;
    cloner* ptr;
    UnicodeString input;
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 2)
        return enif_make_badarg(env);

    /* Second argument must be a binary  */
    if(!(enif_inspect_binary(env, argv[1], &in)  /* Subject */
      && enif_get_resource(env, argv[0], regex_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }

    re = (RegexPattern*) cloner_get(ptr);
    CHECK_RES(env, re);

    input = binary_to_string(in);
    rm = re->matcher((const UnicodeString) input, status);
    CHECK(env, status);

    tail = enif_make_list(env, 0);

    while(rm->find()) {
        head = do_regex_match(env, rm, status);
        CHECK_DEST(env, status,
            delete rm;
        );
        tail = enif_make_list_cell(env, head, tail);
    }
    delete rm;

    return tail;
}

ERL_NIF_TERM regex_match(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ERL_NIF_TERM out;
    RegexPattern* re;
    RegexMatcher* rm;
    cloner* ptr;
    UnicodeString input;
    UErrorCode status = U_ZERO_ERROR;

    if (argc != 2)
        return enif_make_badarg(env);

    /* Second argument must be a binary  */
    if(!(enif_inspect_binary(env, argv[1], &in)  /* Subject */
      && enif_get_resource(env, argv[0], regex_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }

    re = (RegexPattern*) cloner_get(ptr);
    CHECK_RES(env, re);

    input = binary_to_string(in);
    rm = re->matcher((const UnicodeString) input, status);
    CHECK(env, status);


    if(rm->find()) {
        out = do_regex_match(env, rm, status);
        CHECK_DEST(env, status,
            delete rm;
        );
    } else {
        out = enif_make_list(env, 0);
    }
    delete rm;

    return out;
}

int i18n_regex_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    regex_type = enif_open_resource_type(env, NULL, "regex_type",
        regex_dtor, flags, NULL); 
    if (regex_type == NULL) return 6;
    return 0;
}
#endif
