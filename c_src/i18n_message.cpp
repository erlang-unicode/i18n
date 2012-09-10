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
#include "i18n_message.h"

#if I18N_MESSAGE

/* Non-localized realization */
inline void append_uint(unsigned int n, UnicodeString& s)
{
    int tenth;

    do {
        tenth = n / 10;
        s.append((UChar)(n - 10 * tenth + '0'));
        n = tenth;
    } while (n != 0);
}

inline static void append_atom(char * atom, UnicodeString& s)
{
    while (*atom) {
        s.append((UChar) *atom);
        atom++;
    } 
}



static UBool search_in_array(
    const UnicodeString* arr,
    int32_t count,
    const UnicodeString& str,
    unsigned int& i,
    UErrorCode& status) {

    if ((arr == NULL) || (count <= 0) || U_FAILURE(status)) {
        status = U_ILLEGAL_ARGUMENT_ERROR;
        return FALSE;
    }

    for (i = 0; i < (unsigned int) count; i++) 
        if ((str.compare(arr[i])) == 0)
            return TRUE;

    return FALSE;
}


#ifdef SKIP_MESSAGE_PATTERN
/* Return TRUE if the elem was found. */
static UnicodeString* enum_to_array(
    StringEnumeration& en,
    int32_t& count,
    UErrorCode& status) {

    int32_t index = 0;

    en.reset(status);
    if (U_FAILURE(status))
        return NULL;

    count = en.count(status);
    if (U_FAILURE(status))
        return NULL;

    if (count == 0)
        return NULL;

    UnicodeString* arr = new UnicodeString[count];
    const UnicodeString* s;


    while ((s = en.snext(status)) != NULL) {
        if (U_FAILURE(status))
            goto failure;

        arr[index].append(*s);
        index++;
    }
    if (index != count)
        goto failure;

    return arr;

    failure:
        delete[] arr;
        return NULL;
}

#else

static const int32_t 
explicitTypesCount = 4;

static const
UnicodeString 
explicitTypes[] = {
    UnicodeString("number"),
    UnicodeString("date"),
    UnicodeString("time"),
    UnicodeString("datetime"),
};

static const Formattable::Type
explicitTypesIndex[] = {
    Formattable::kDouble,
    Formattable::kDate,
    Formattable::kDate,
    Formattable::kDate,
};

inline Formattable::Type
parseArgType(
    MessagePattern& p, 
    const MessagePattern::Part& part, 
    int32_t i,
    UErrorCode& status) 
{
    UMessagePatternArgType argType = part.getArgType();

    switch (argType) {
    case UMSGPAT_ARG_TYPE_NONE:
        return Formattable::kString;
        break;
    case UMSGPAT_ARG_TYPE_SIMPLE: { 
        UnicodeString explicitType = 
            p.getSubstring(p.getPart(i+2));

        unsigned int index;
        UBool
        is_found = search_in_array(explicitTypes, explicitTypesCount, 
            explicitType, index, status);
       
        if (U_FAILURE(status) || (is_found == FALSE)) {
            status = U_INTERNAL_PROGRAM_ERROR;
            return Formattable::kString;
        }
        
        return explicitTypesIndex[index];
        break;
    }
    case UMSGPAT_ARG_TYPE_CHOICE:
    case UMSGPAT_ARG_TYPE_PLURAL:
        return Formattable::kDouble;
        break;
    case UMSGPAT_ARG_TYPE_SELECT:
        return Formattable::kString;
        break;
    default:
        status = U_INTERNAL_PROGRAM_ERROR;  // Should be unreachable.
        return Formattable::kString;
        break;
    }
}
#endif



U_NAMESPACE_BEGIN

/**
 * This class isolates our access to private internal methods of
 * MessageFormat.  It is never instantiated; it exists only for C++
 * access management.
 *
 * After version 4.8 this class also has old version of `getFormatNames'.
 */
class MessageFormatAdapter {
public:
static void getFormat(
    MessageFormat& m, 
    UnicodeString*& names,
    Formattable::Type*& types,
    int32_t& count, 
    UErrorCode& status) {

    names = NULL; 
    types = NULL;
    
    UBool named = m.usesNamedArguments();




    /* Before 4.8 */
#ifdef SKIP_MESSAGE_PATTERN
    // Clone names

    count = m.getArgTypeCount();
    if (count == 0) 
        return;

    if (named) {
        StringEnumeration* en;
        en = m.getFormatNames(status);

        if (U_FAILURE(status))
            return;

        names = enum_to_array(*en, count, status);
        delete en;

        if (U_FAILURE(status)) {
            names = NULL;
            return;
        }
    } else {
        names = NULL;
    }


    // Clone types
    types = new Formattable::Type[count];

    const Formattable::Type* src_types;
    src_types = m.getArgTypeList(count);

    for (int i = 0; i<count; i++)
        types[i] = src_types[i];

    return;

#else

    /* In 4.8 */


    if (!named) {
        count = m.getArgTypeCount();
        if (count == 0) 
            return;

        types = new Formattable::Type[count];

        const Formattable::Type* src_types;
        src_types = m.getArgTypeList(count);

        for (int i = 0; i<count; i++)
            types[i] = src_types[i];

        names = NULL;
        return;
    }

    

    
    

    MessagePattern& p = m.msgPattern;

    int32_t limit = p.countParts();

    count = 0;
    for (int32_t i = 0; i < limit; ++i) {
        const MessagePattern::Part& part = p.getPart(i);

        if (part.getType() ==  UMSGPAT_PART_TYPE_ARG_START)
            count++;
    }
    
    if (count == 0)
        return;

    names = new UnicodeString[count];
    types = new Formattable::Type[count];

    

    int32_t num = 0;

    for (int32_t i = 0; i < limit && U_SUCCESS(status); ++i) {
        const MessagePattern::Part& part = p.getPart(i);

        switch (part.getType()) {
        case UMSGPAT_PART_TYPE_ARG_START:
            types[num] = parseArgType(p, part, i, status);
            break;

        case UMSGPAT_PART_TYPE_ARG_LIMIT:
            num++;
            break;

        case UMSGPAT_PART_TYPE_ARG_NAME:
            names[num].append(p.getSubstring(part));
            break;

        case UMSGPAT_PART_TYPE_ARG_NUMBER:
            // TODO: check negative values
            unsigned int value;
            value = (unsigned int) part.getValue(); 
            append_uint(value, names[num]);
            break;
            
        default:
            ;
        }
    };

    if ((num != count) && U_SUCCESS(status)) {
        status = U_INTERNAL_PROGRAM_ERROR;
        status = U_ILLEGAL_ARGUMENT_ERROR;
    }

    if (U_FAILURE(status)) {
        delete[] names;
        names = NULL;
    }
#endif
}

};


U_NAMESPACE_END

U_NAMESPACE_USE



ERL_NIF_TERM test_internal_format_num_id(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{
    UErrorCode status = U_ZERO_ERROR;
    UnicodeString* names;
    Formattable::Type* types;
    int32_t count;

    MessageFormat diskForm(
        "The disk \"{1}\" contains {0} file(s).", status );

    CHECK(env, status);

    MessageFormatAdapter::getFormat(
        diskForm, 
        names,
        types,
        count, 
        status);

    CHECK(env, status);

    if (names != NULL)
        ERROR_STRING(env, "Pointer must be NULL for number names.");

    if (count != 2)
        ERROR_STRING(env, "Bad count value.");

    if (types[0] != Formattable::kString)
        ERROR_STRING(env, "Bad Formattable::Type.");

    if (types[1] != Formattable::kString)
        ERROR_STRING(env, "Bad Formattable::Type.");

    return ATOM_TRUE;
}

ERL_NIF_TERM test_internal_format_name_id(ErlNifEnv* env, int /*argc*/, 
    const ERL_NIF_TERM /*argv*/[])
{

    UErrorCode status = U_ZERO_ERROR;
    UnicodeString* names;
    Formattable::Type* types;
    int32_t count;

    MessageFormat diskForm(
        "The disk \"{disk}\" contains {count} file(s).", status );

    CHECK(env, status);

    MessageFormatAdapter::getFormat(
        diskForm, 
        names,
        types,
        count, 
        status);

    CHECK(env, status);

    if (names == NULL)
        ERROR_STRING(env, "Pointer must be not NULL for string names.");

    if (count != 2)
        ERROR_STRING(env, "Bad count value.");


    UBool is_found;    
    unsigned int index;

    UnicodeString goal = UnicodeString("disk");
    is_found = search_in_array(names, count, goal, index, status);
    if (U_FAILURE(status) || (is_found == FALSE) || (index != 0))
        ERROR_STRING(env, "search_in_array failured.");

    goal = UnicodeString("count");
    is_found = search_in_array(names, count, goal, index, status);
    if (U_FAILURE(status) || (is_found == FALSE) || (index != 1))
        ERROR_STRING(env, "search_in_array failured.");

    goal = UnicodeString("bomb");
    is_found = search_in_array(names, count, goal, index, status);
    if (U_FAILURE(status) || (is_found == TRUE))
        ERROR_STRING(env, "search_in_array failured.");

    if (types[0] != Formattable::kString)
        ERROR_STRING(env, "Bad Formattable::Type.");

    if (types[1] != Formattable::kString)
        ERROR_STRING(env, "Bad Formattable::Type.");

    if (names[0] != UnicodeString("disk"))
        ERROR_STRING(env, "Bad name.");

    if (names[1] != UnicodeString("count"))
        ERROR_STRING(env, "Bad name.");

    return ATOM_TRUE;
}

typedef struct {
    struct cloner_store cloner;
    UnicodeString* names;
    Formattable::Type* types;
    int32_t count;
} IMessage;


static ErlNifResourceType* message_type = 0;


/* Called from erl_nif. */
static void message_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    IMessage* res = (IMessage*) obj;
    if (res->names != NULL)
        delete[] res->names;

    cloner_destroy(&(res->cloner)); 
}



/* Called from cloner for each thread. */
static void message_close(char* ptr) 
{ 
    delete (MessageFormat*) ptr;
}

static char* message_clone(char* ptr) 
{
    if (ptr == NULL) 
        return NULL;

    MessageFormat* from = (MessageFormat*) ptr;
    Format* to = from->clone();
    return (char*) to;
}



static int message_open(MessageFormat * msg, IMessage* res)
{
    UErrorCode status = U_ZERO_ERROR;
    MessageFormatAdapter::getFormat(
        *msg, 
        res->names,
        res->types,
        res->count, 
        status);
      
    if (U_FAILURE(status)) {
        return 1;
    }

    //TODO: check status
    return cloner_open((char *) msg, &(res->cloner), 
            &message_clone, &message_close);
} 








/**
 * NIFs
 */

/* Get a message format 
 * i18n:to(i18n_message:format(i18n_message:open(i18n:from("{0,date}")),
 * [{i18n:from("0"), i18n_nif:date_now()}])).  
 * <<"2011 9 28">> */

ERL_NIF_TERM open_format(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out;
    ErlNifBinary in;
    char locale[LOCALE_LEN];
    UErrorCode status = U_ZERO_ERROR;
    UParseError pe;
    UMessageFormat* msg;
    IMessage* res;

    if (argc != 2)
        return enif_make_badarg(env);

    if (!(enif_get_atom(env, argv[0], (char*) locale, 
                LOCALE_LEN, ERL_NIF_LATIN1)
          && enif_inspect_binary(env, argv[1], &in))) {
        return enif_make_badarg(env);
    }

    /* get a message format */
    msg = umsg_open((UChar *) in.data, 
            TO_ULEN(in.size),
            (char *) locale, 
            &pe, 
            &status);
    if (U_FAILURE(status)) {
        ERROR_PARSE(env, status, &pe);
    }


    res = (IMessage*) enif_alloc_resource(message_type, 
            sizeof(IMessage));
    if (message_open((MessageFormat*) msg, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}

unsigned int butoui(ErlNifBinary& bu, UErrorCode& status)
{
    unsigned int i = 0;
    int32_t len = TO_ULEN(bu.size);
    
    UChar* pos = (UChar*) (bu.data);
    UChar ch;

    while (len) {
        ch = *pos;
        if ((ch<'0') || (ch>'9')) {
            /* not number */
            status = U_ILLEGAL_ARGUMENT_ERROR;
            return i;
        }
        i *= 10;
        i += ch-'0';
        

        len--;
        pos++;
    }
    return i;
}

unsigned int stoui(char* pos, UErrorCode& status)
{
    unsigned int i = 0;
    
    char ch;

    while (* pos) {
        ch = *pos;
        if ((ch<'0') || (ch>'9')) {
            /* not number */
            status = U_ILLEGAL_ARGUMENT_ERROR;
            return i;
        }
        i *= 10;
        i += ch-'0';
        
        pos++;
    }
    return i;
}

/* Returns TRUE if success */
static UBool
parseNameId(ErlNifEnv* env, const ERL_NIF_TERM term, 
    UnicodeString& name)
{
    ErlNifBinary bin;

    /* [..., {Id, Arg}, ...] */
    /* Inspect first element of the tuple (extract the name) */

    if (enif_inspect_binary(env, term, &bin)) {
        /* typeof(Id) == unicode_string */
        name.append((UChar*) bin.data, 
            (int32_t) TO_ULEN(bin.size));
    } else 

    if (enif_is_atom(env, term)) {
        /* typeof(Id) == atom */
        char atom[ATOM_LEN];
        if (!enif_get_atom(env, term, (char*) atom, ATOM_LEN,
            ERL_NIF_LATIN1))
            return FALSE;
        
        append_atom((char *) atom, name);
    } else 
        return FALSE;

    return TRUE;
}

/* Returns the parsed value */
static unsigned int
parseNumId(ErlNifEnv* env, const ERL_NIF_TERM term, 
    UErrorCode& status)
{
    int tInt;

    /* Inspect first element of the tuple (extract the name) */
    if (enif_get_int(env, term, &tInt)) {
        /* typeof(Id) == integer */
        return (unsigned int) tInt;
    } else 

    if (enif_is_binary(env, term)) {
        ErlNifBinary bin;
        if (enif_inspect_binary(env, term, &bin)) 
            /* typeof(Id) == unicode_string */
            return butoui(bin, status);
    } else 

    if (enif_is_atom(env, term)) {
        /* typeof(Id) == atom */
        char atom[ATOM_LEN];
        if (enif_get_atom(env, term, (char*) atom, ATOM_LEN,
            ERL_NIF_LATIN1))
            return stoui((char *) atom, status);
    }  

   status = U_ILLEGAL_ARGUMENT_ERROR;
    return 0;
}

/* Returns TRUE if success */
static UBool
fillValue(ErlNifEnv* env, Formattable::Type type, 
    const ERL_NIF_TERM from, Formattable& to)
{
    double tDouble;
    int tInt;
    ErlNifSInt64 tInt64;
    ErlNifBinary bin;

    switch (type) {
    
    case Formattable::kDate:

        if (!enif_get_double(env, from, &tDouble)) 
            return FALSE;

        to.setDate((UDate) tDouble);
        break;
    
    case Formattable::kDouble:

        if (!enif_get_double(env, from, &tDouble)) 
            return FALSE;

        to.setDouble(tDouble);
        break;
    
    case Formattable::kLong:

        if (!enif_get_int(env, from, &tInt))
            return FALSE;

        to.setLong((int32_t) tInt);
        break;

    case Formattable::kInt64:

        if (!enif_get_int64(env, from, &tInt64)) 
            return FALSE;

        to.setInt64((int64_t) tInt64);
        break;
        
    case Formattable::kString:

        if (!enif_inspect_binary(env, from, &bin)) 
            return FALSE;

        to.setString(
            * new UnicodeString(
                (const UChar*) bin.data, 
                (int32_t) TO_ULEN(bin.size)));
        break;

    default:
        return FALSE;
    }

    return TRUE;
}


ERL_NIF_TERM format(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ERL_NIF_TERM out, list;
    int32_t len; 
    unsigned int count, i, pos;
    UErrorCode status = U_ZERO_ERROR;
    UnicodeString appendTo;
    IMessage* obj;
    MessageFormat* msg;
    
    ERL_NIF_TERM* tuple;

    

    if ((argc != 2) && (argc != 3))
        return enif_make_badarg(env);

    if(!(enif_get_list_length(env, argv[1], &count)
      && enif_get_resource(env, argv[0], message_type, (void**) &obj))) {
        return enif_make_badarg(env);
    }

    msg = (MessageFormat*) cloner_get(&(obj->cloner));
    CHECK_RES(env, msg);


    if (argc == 3) {
        /* Pass the start of the string as the third argument */
        if (enif_inspect_binary(env, argv[2], &in)) {
            appendTo.append((UChar*) in.data, 
                TO_ULEN(in.size));
        } else {
            /* Third elem is not a string */
            return enif_make_badarg(env);
        }
    }

    /* Allocate at least one element.  Allocating an array of length
     * zero causes problems on some platforms (e.g. Win32).
     */
    Formattable* args = new Formattable[count ? count : 1];
    UnicodeString* names = NULL;

    /* i is the number of the element of the list. */
    i = 0;
    list = argv[1];


    if (obj->names == NULL) {
        /* Numeric format */
        while (enif_get_list_cell(env, list, &out, &list)) {
            if (enif_get_tuple(env, out, &len, 
                    (const ERL_NIF_TERM**) &tuple)
                    && (len == 2)) {
                pos = parseNumId(env, tuple[0], status);

                if (U_FAILURE(status) || (((int) pos) >= (obj->count)))
                    goto bad_elem;

                /* Set formatttable. */
                if (!fillValue(env, obj->types[pos], tuple[1], args[pos]))
                    goto bad_elem;
                
            } else {
                /* Set formatttable. */
                if (!fillValue(env, obj->types[i], out, args[i]))
                    goto bad_elem;
            }
        
            i++;
        }
    } else { 
        /* Name format */
        names = new UnicodeString[count ? count : 1];

        while (enif_get_list_cell(env, list, &out, &list)) {
            if (!(enif_get_tuple(env, out, &len, 
                    (const ERL_NIF_TERM**) &tuple)
                    && (len == 2))) 
                goto bad_elem;

            if (!parseNameId(env, tuple[0], names[i]))
                goto bad_elem;
                
            /* Read the element name from the array. 
             * Set pos. */
            UBool is_found;
            is_found = search_in_array(
                obj->names,
                obj->count,
                names[i],
                pos,
                status);

            if (U_FAILURE(status) || (is_found == FALSE))
                goto handle_error;

            if (pos >= (unsigned int) obj->count) 
                goto handle_error;


            /* Set formatttable. */
            if (!fillValue(env, obj->types[pos], tuple[1], args[i]))
                goto bad_elem;
        
            i++;
        }
    }

    
    msg->format(
        (const UnicodeString *) names,
        (const Formattable *) args,
        (int32_t) count,
        appendTo,
        status      
    );
    delete[] args;
    if (names != NULL)
        delete[] names;

    CHECK(env, status);

    return string_to_term(env, appendTo);

    bad_elem:
        /* Memory deallocation */
        delete[] args;
        if (names != NULL)
            delete[] names;
        ERROR_ELEMENT(env, status, argv[1], i);

    handle_error:
        /* Memory deallocation */
        delete[] args;
        if (names != NULL)
            delete[] names;
        ERROR(env, status);
}







int i18n_message_load(ErlNifEnv *env, void ** /*priv_data*/, 
    ERL_NIF_TERM /*load_info*/)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    message_type = enif_open_resource_type(env, NULL, "message_type",
        message_dtor, flags, NULL); 
    if (message_type == NULL) return 5;
    return 0;
}
#endif
