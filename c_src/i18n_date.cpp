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
#include "i18n_date.h"
#include "i18n_collation.h"
#include "i18n_search.h"
#include "i18n_message.h"
#include "i18n_regex.h"
#include "i18n_locale.h"



#if I18N_DATE

static ErlNifResourceType* calendar_type = 0;


/* Called from erl_nif. */
static void calendar_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
static void calendar_close(char* obj) 
{ 
    if (obj != NULL)
        ucal_close((UCalendar*) obj);
}
static char* calendar_clone(char* obj) 
{
    UErrorCode status = U_ZERO_ERROR;
    
    obj = (char*) ucal_clone((UCalendar*) obj, &status);
    if(U_FAILURE(status)) { 
        return NULL;
    } 

    return obj;
}

static int calendar_open(UCalendar * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &calendar_clone, &calendar_close);
} 







/**
 * NIFs
 */


inline static UCalendarType parserCalendarType(const char * type) 
{
    return (!strcmp((char*) "gregorian",   type)) ? UCAL_GREGORIAN :
           (!strcmp((char*) "traditional", type)) ? UCAL_TRADITIONAL :
            UCAL_DEFAULT;
}
ERL_NIF_TERM open_calendar(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out;
    ErlNifBinary tz;

    cloner* res;
    UCalendar* cal;
    UCalendarType type = UCAL_DEFAULT;
    UErrorCode status = U_ZERO_ERROR;
    char type_atom[ATOM_LEN], locale[LOCALE_LEN];
    
    tz.size = 0; 
    tz.data = 0;

    switch (argc) {
        case 3:
            if (!enif_get_atom(env, argv[2], (char*) type_atom, ATOM_LEN,
                ERL_NIF_LATIN1))
                return enif_make_badarg(env);
            type = parserCalendarType((const char *) type_atom);
        case 2:
            if (!enif_inspect_binary(env, argv[1], &tz))
                return enif_make_badarg(env);
        case 1:
            if (!enif_get_atom(env, argv[0], (char*) locale, 
                    LOCALE_LEN, ERL_NIF_LATIN1))
                return enif_make_badarg(env);
        break;
        default:
            return enif_make_badarg(env);
    }


    /* get a calendar type */
    cal = ucal_open(
        (const UChar *) tz.data,
        (int32_t) tz.size,
        (const char *) locale,
        type,
        &status);
    CHECK(env, status);


    res = (cloner*) enif_alloc_resource(calendar_type, sizeof(cloner));
    if (calendar_open(cal, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}

static int parseCalendarDateField(const char * type) 
{
    return (!strcmp((char*) "era",           type)) ? UCAL_ERA :
           (!strcmp((char*) "year",          type)) ? UCAL_YEAR :
           (!strcmp((char*) "month",         type)) ? UCAL_MONTH :
           (!strcmp((char*) "week_of_year",  type)) ? UCAL_WEEK_OF_YEAR :
           (!strcmp((char*) "week_of_month", type)) ? UCAL_WEEK_OF_MONTH :
           (!strcmp((char*) "date",          type)) ? UCAL_DATE :
           (!strcmp((char*) "day",           type)) ? UCAL_DATE :
           (!strcmp((char*) "day_of_year",   type)) ? UCAL_DAY_OF_YEAR :
           (!strcmp((char*) "day_of_week",   type)) ? UCAL_DAY_OF_WEEK :
           (!strcmp((char*) "am_pm",         type)) ? UCAL_AM_PM :
           (!strcmp((char*) "hour",          type)) ? UCAL_HOUR :
           (!strcmp((char*) "hour_of_day",   type)) ? UCAL_HOUR_OF_DAY :
           (!strcmp((char*) "minute",        type)) ? UCAL_MINUTE :
           (!strcmp((char*) "second",        type)) ? UCAL_SECOND :
           (!strcmp((char*) "millisecond",   type)) ? UCAL_MILLISECOND :
           (!strcmp((char*) "zone_offset",   type)) ? UCAL_ZONE_OFFSET :
           (!strcmp((char*) "dst_offset",    type)) ? UCAL_DST_OFFSET :
           (!strcmp((char*) "day_of_week_in_month", type))  
                ? UCAL_DAY_OF_WEEK_IN_MONTH :
            -1;
}

typedef void (*date_fun_ptr)(
    UCalendar *,
    UCalendarDateFields,
    int32_t,
    UErrorCode *
    );
inline static ERL_NIF_TERM do_offset(ErlNifEnv* env, 
    UCalendar* cal,
    date_fun_ptr fun,
    const ERL_NIF_TERM in) 
{
    UCalendarDateFields field;
    UErrorCode status = U_ZERO_ERROR;

    ERL_NIF_TERM head, tail;
    ERL_NIF_TERM* tuple;
    unsigned int count, i;
    int32_t len, offset; 

    char    value[ATOM_LEN];
    int     parsed_value;
    
    
    i = 0;
    if (!enif_get_list_length(env, in, &count)) 
        return enif_make_badarg(env);

    tail = in;

    while (enif_get_list_cell(env, tail, &head, &tail)) {

        if (enif_get_tuple(env, head, &len, (const ERL_NIF_TERM**) &tuple)
            && (len == 2)) { 

            /* Set an attribute start */

            if (!(enif_get_atom(env, tuple[0], (char*) value, 
                        ATOM_LEN, ERL_NIF_LATIN1) 
               && enif_get_int(env, tuple[1], &offset))) 
                goto bad_elem;
                
            parsed_value = parseCalendarDateField(value);
            if ((parsed_value == -1)) 
                goto bad_elem;

            field = (UCalendarDateFields) parsed_value;
 
            fun(cal, field, offset, &status);

            if (U_FAILURE(status))
                goto bad_elem;
            
            /* Set an attribute end */

        } else 
            goto bad_elem;
    }


    return calendar_to_double(env, (const UCalendar*) cal);

    bad_elem:
        return list_element_error(env, in, i);
}

static void do_ucal_set(UCalendar * cal,
        UCalendarDateFields field,
        int32_t amount,
        UErrorCode * /*status*/) {
    if (field == UCAL_MONTH)
        amount--; /* month from 0 */
    ucal_set(cal, field, amount);
} 

ERL_NIF_TERM date_set(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    double date;

    if(!((argc == 3)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_double(env, argv[1], &date))) {
        return enif_make_badarg(env);
    }

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    ucal_setMillis(cal, (UDate) date, &status);
    CHECK(env, status);

    return do_offset(env, cal, do_ucal_set, argv[2]);
}

ERL_NIF_TERM date_add(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    double date;

    if(!((argc == 3)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_double(env, argv[1], &date))) {
        return enif_make_badarg(env);
    }

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    ucal_setMillis(cal, (UDate) date, &status);
    CHECK(env, status);

    return do_offset(env, cal, ucal_add, argv[2]);
}

ERL_NIF_TERM date_roll(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    double date;

    if(!((argc == 3)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_double(env, argv[1], &date))) {
        return enif_make_badarg(env);
    }

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    ucal_setMillis(cal, (UDate) date, &status);
    CHECK(env, status);

    return do_offset(env, cal, ucal_roll, argv[2]);
}


ERL_NIF_TERM date_clear(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    double date;

    UCalendarDateFields field;
    ERL_NIF_TERM head, tail;
    unsigned int count, i = 0;

    char    value[ATOM_LEN];
    int     parsed_value;

    if(!((argc == 3)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_double(env, argv[1], &date)
      && enif_get_list_length(env, argv[2], &count))) {
        return enif_make_badarg(env);
    }

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    ucal_setMillis(cal, (UDate) date, &status);
    CHECK(env, status);

    tail = argv[2];
    while (enif_get_list_cell(env, tail, &head, &tail)) {

            /* Set an attribute start */

            if (!enif_get_atom(env, head, (char*) value, 
                    ATOM_LEN, ERL_NIF_LATIN1)) 
                goto bad_elem;
                
            parsed_value = parseCalendarDateField(value);
            if ((parsed_value == -1)) 
                goto bad_elem;

            field = (UCalendarDateFields) parsed_value;
 
            ucal_clearField(cal, field);

            if (U_FAILURE(status))
                goto bad_elem;
            
            /* Set an attribute end */

    }

    return calendar_to_double(env, (const UCalendar*) cal);

    bad_elem:
        return list_element_error(env, argv[2], i);
}

static ERL_NIF_TERM do_date_get_field(ErlNifEnv* env, UCalendar* cal,
    const ERL_NIF_TERM field_atom, UErrorCode& status)
{
    char    value[ATOM_LEN];
    int     parsed_value, amount;
    UCalendarDateFields field;

    if (!enif_get_atom(env, field_atom, (char*) value, ATOM_LEN, 
            ERL_NIF_LATIN1)) {
        status = U_ILLEGAL_ARGUMENT_ERROR;
        return 0;
    }

    parsed_value = parseCalendarDateField(value);
    if (parsed_value == -1) {
        status = U_ILLEGAL_ARGUMENT_ERROR;
        return 0;
    }

    field = (UCalendarDateFields) parsed_value;

    amount = (int) ucal_get(cal, field, &status);
    if (U_FAILURE(status))
        return 0;

    if (field == UCAL_MONTH)
        amount++; /* month from 0 */

    return enif_make_int(env, amount);
}

ERL_NIF_TERM date_get_field(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    double date;
    ERL_NIF_TERM res;


    if(!((argc == 3)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_double(env, argv[1], &date))) {
        return enif_make_badarg(env);
    }

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    ucal_setMillis(cal, (UDate) date, &status);
    CHECK(env, status);

    res = do_date_get_field(env, cal, argv[2], status);
    CHECK(env, status);

    return res;
}

ERL_NIF_TERM date_get_fields(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    double date;
    ERL_NIF_TERM res;

    ERL_NIF_TERM head, tail, out;
    unsigned int count;


    if(!((argc == 3)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_double(env, argv[1], &date)
      && enif_get_list_length(env, argv[2], &count))) {
        return enif_make_badarg(env);
    }

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    ucal_setMillis(cal, (UDate) date, &status);
    CHECK(env, status);

    tail = argv[2];
    out = enif_make_list(env, 0);
    while (enif_get_list_cell(env, tail, &head, &tail)) {

            /* Set an attribute start */
            res = do_date_get_field(env, cal, head, status);
            CHECK(env, status);
            out = enif_make_list_cell(env, 
                    enif_make_tuple2(env, head, res),
                    out);

            /* Set an attribute end */

    }

    return out;
}

ERL_NIF_TERM date_now(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return enif_make_double(env, (double) ucal_getNow());
}

ERL_NIF_TERM date_is_weekend(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    double date;
    UBool flag;

    if(!((argc == 2)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_double(env, argv[1], &date))) {
        return enif_make_badarg(env);
    }

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    flag = ucal_isWeekend(cal, (UDate) date, &status);
    CHECK(env, status);

    return bool_to_term(flag);
}


ERL_NIF_TERM date_get3(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    int32_t year, month, day;    

    if(!((argc == 4)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_int(env, argv[1], &year)  
      && enif_get_int(env, argv[2], &month)  
      && enif_get_int(env, argv[3], &day))) {
        return enif_make_badarg(env);
    }
    month--;

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    ucal_setDate(cal,
        year,
        month,
        day,
        &status);
    CHECK(env, status);

    return calendar_to_double(env, (const UCalendar*) cal);
}

ERL_NIF_TERM date_get6(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;
    UCalendar* cal;
    cloner* ptr;
    int32_t year, month, day, hour, minute, second;    

    if(!((argc == 7)
      && enif_get_resource(env, argv[0], calendar_type, (void**) &ptr)  
      && enif_get_int(env, argv[1], &year)  
      && enif_get_int(env, argv[2], &month)  
      && enif_get_int(env, argv[3], &day)  
      && enif_get_int(env, argv[4], &hour)  
      && enif_get_int(env, argv[5], &minute)  
      && enif_get_int(env, argv[6], &second))) {
        return enif_make_badarg(env);
    }
    month--;

    cal = (UCalendar*) cloner_get(ptr);
    CHECK_RES(env, cal);

    ucal_setDateTime(cal,
        year,
        month,
        day,
        hour,
        minute,
        second,
        &status);
    CHECK(env, status);

    return calendar_to_double(env, (const UCalendar*) cal);
}


ERL_NIF_TERM calendar_locales(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return generate_available(env, ucal_getAvailable, 
            ucal_countAvailable());
}

int i18n_date_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    calendar_type = enif_open_resource_type(env, NULL, "calendar_type",
        calendar_dtor, flags, NULL); 
    if (calendar_type == NULL) return 6;
    return 0;
}
#endif
