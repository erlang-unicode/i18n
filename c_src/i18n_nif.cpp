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


#define I18N_INFO       0

#define I18N_STRING     1
#define I18N_COLLATION  1
#define I18N_SEARCH    	1
#define I18N_MESSAGE   	1
#define I18N_REGEX   	1
#define I18N_LOCALE   	1
#define I18N_DATE   	1
#define I18N_TRANS  	1

/* 2^16, since we're using a 2-byte length header */
#define BUF_SIZE        65536 
#define STR_LEN         32768
#define LOCALE_LEN      255
#define ATOM_LEN        16


#if I18N_LOCALE
#include "unicode/uloc.h"
#endif

#include "unicode/ustring.h"
//#include "unicode/uchar.h"

#if I18N_STRING
#include "unicode/ubrk.h"
#include "unicode/unorm2.h"
#endif

#if I18N_COLLATION
#include "unicode/ucol.h"
#include "unicode/coll.h"
#endif

#if I18N_SEARCH
#include "unicode/usearch.h"
#endif

#if I18N_MESSAGE
#include "unicode/umsg.h"
#include "unicode/msgfmt.h"
#endif

#include "unicode/utypes.h"

#if I18N_REGEX
#include "unicode/regex.h"
#endif

#if I18N_DATE
#include "unicode/ucal.h"
#endif

#if I18N_TRANS
#include "unicode/utrans.h"
#include "unicode/translit.h"
#include "unicode/uenum.h"
#endif



#include "erl_nif.h"

extern "C" {
#include "cloner.h"
}

#include <string.h>

#define CHECK(ENV, X) \
    if (U_FAILURE(X)) return get_error_code(ENV, X); 

/* If error, run the destructor DEST */
#define CHECK_DEST(ENV, X, DEST) \
    if (U_FAILURE(X)) {DEST; return get_error_code(ENV, X);}


/* Divide by 2 */
#define TO_ULEN(X)   ((X) / sizeof(UChar))

/* Multiply by 2 */
#define FROM_ULEN(X) ((X) * sizeof(UChar))

#define ERROR(ENV, X) return get_error_code(ENV, X);

ERL_NIF_TERM res_error_term;
#define CHECK_RES(ENV, RES) if (RES == NULL) return res_error_term;



/* Allocated atoms */
ERL_NIF_TERM ATOM_TRUE, ATOM_FALSE;
ERL_NIF_TERM ATOM_EQUAL, ATOM_GREATER, ATOM_LESS;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ENDIAN;

ERL_NIF_TERM ATOM_COUNT;
ERL_NIF_TERM ATOM_RESOURCE;
ERL_NIF_TERM ATOM_SEARCH;




static ERL_NIF_TERM make_error(ErlNifEnv* env, const char* code) {
    return enif_make_tuple2(env,
        enif_make_atom(env, "error"),
        enif_make_atom(env, code));
}

static ERL_NIF_TERM parse_error(ErlNifEnv* env, const char* code, 
        UParseError* e) {
    return enif_make_tuple4(env,
        enif_make_atom(env, "error"), 
        enif_make_atom(env, code),
        enif_make_tuple2(env,
            enif_make_atom(env, "line"),
            enif_make_int(env, (int) e->line)),
        enif_make_tuple2(env,
            enif_make_atom(env, "offset"),
            enif_make_int(env, (int) e->offset))
        );
}

static ERL_NIF_TERM list_element_error(ErlNifEnv* env, 
    const ERL_NIF_TERM list, int32_t num) {
    return enif_make_tuple4(env,
        enif_make_atom(env, "error"),
        enif_make_atom(env, "bad_element"),
        enif_make_tuple2(env,
            enif_make_atom(env, "list"),
            list),
        enif_make_tuple2(env,
            enif_make_atom(env, "index"),
            enif_make_int(env, (int) num))
        );
}


static int i18n_atom_load(ErlNifEnv *env, void ** /*priv_data*/, 
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

    res_error_term = make_error(env, "i18n_resource_error");

    return 0;
}




/** 
 * http://icu-project.org/apiref/icu4c/utypes_8h.html#a3343c1c8a8377277046774691c98d78c
 */
static ERL_NIF_TERM get_error_code(ErlNifEnv* env, UErrorCode status) {
    switch (status) {
#if U_ILLEGAL_ARGUMENT_ERROR
        case U_ILLEGAL_ARGUMENT_ERROR:
            return make_error(env, "i18n_illegal_argument_error");
#endif
#if U_MISSING_RESOURCE_ERROR
        case U_MISSING_RESOURCE_ERROR:
            return make_error(env, "i18n_resourse_error");
#endif
#if U_INVALID_FORMAT_ERROR
        case U_INVALID_FORMAT_ERROR:
            return make_error(env, "i18n_format_error");
#endif
#if U_FILE_ACCESS_ERROR
        case U_FILE_ACCESS_ERROR:
            return make_error(env, "i18n_access_error");
#endif
#if U_INTERNAL_PROGRAM_ERROR
        case U_INTERNAL_PROGRAM_ERROR:
            return make_error(env, "i18n_internal_program_error");
#endif
#if U_MESSAGE_PARSE_ERROR
        case U_MESSAGE_PARSE_ERROR:
            return make_error(env, "i18n_parse_error");
#endif
#if U_MEMORY_ALLOCATION_ERROR
        case U_MEMORY_ALLOCATION_ERROR:
            return make_error(env, "i18n_memory_allocation_error");
#endif
#if U_INDEX_OUTOFBOUNDS_ERROR
        case U_INDEX_OUTOFBOUNDS_ERROR:
            return make_error(env, "i18n_index_out_of_bounds_error");
#endif
#if U_INVALID_CHAR_FOUND
        case U_INVALID_CHAR_FOUND:
            return make_error(env, "i18n_invalid_char_found");
#endif
#if U_TRUNCATED_CHAR_FOUND
        case U_TRUNCATED_CHAR_FOUND:
            return make_error(env, "i18n_truncated_char_found");
#endif
#if U_ILLEGAL_CHAR_FOUND
        case U_ILLEGAL_CHAR_FOUND:
            return make_error(env, "i18n_illegal_char_found");
#endif
#if U_INVALID_TABLE_FORMAT
        case U_INVALID_TABLE_FORMAT:
            return make_error(env, "i18n_invalid_table_format");
#endif
#if U_INVALID_TABLE_FILE
        case U_INVALID_TABLE_FILE:
            return make_error(env, "i18n_invalid_table_file");
#endif
#if U_BUFFER_OVERFLOW_ERROR
        case U_BUFFER_OVERFLOW_ERROR:
            return make_error(env, "i18n_buffer_overflow_error");
#endif
#if U_UNSUPPORTED_ERROR
        case U_UNSUPPORTED_ERROR:
            return make_error(env, "i18n_unsupported_error");
#endif
#if U_RESOURCE_TYPE_MISMATCH
        case U_RESOURCE_TYPE_MISMATCH:
            return make_error(env, "i18n_resource_type_mismatch");
#endif
#if U_ILLEGAL_ESCAPE_SEQUENCE
        case U_ILLEGAL_ESCAPE_SEQUENCE:
            return make_error(env, "i18n_escape_sequence");
#endif
#if U_UNSUPPORTED_ESCAPE_SEQUENCE
        case U_UNSUPPORTED_ESCAPE_SEQUENCE:
            return make_error(env, "i18n_unsupported_escape_sequence");
#endif
#if U_NO_SPACE_AVAILABLE
        case U_NO_SPACE_AVAILABLE:
            return make_error(env, "i18n_no_space_available");
#endif
#if U_CE_NOT_FOUND_ERROR
        case U_CE_NOT_FOUND_ERROR:
            return make_error(env, "i18n_ce_not_found_error");
#endif
#if U_PRIMARY_TOO_LONG_ERROR
        case U_PRIMARY_TOO_LONG_ERROR:
            return make_error(env, "i18n_primary_too_long_error");
#endif
#if U_STATE_TOO_OLD_ERROR
        case U_STATE_TOO_OLD_ERROR:
            return make_error(env, "i18n_state_too_old_error");
#endif
#if U_TOO_MANY_ALIASES_ERROR
        case U_TOO_MANY_ALIASES_ERROR:
            return make_error(env, "i18n_too_many_aliases_error");
#endif
#if U_ENUM_OUT_OF_SYNC_ERROR
        case U_ENUM_OUT_OF_SYNC_ERROR:
            return make_error(env, "i18n_enum_out_of_sync_error");
#endif
#if U_INVARIANT_CONVERSION_ERROR
        case U_INVARIANT_CONVERSION_ERROR:
            return make_error(env, "i18n_invariant_conversion_error");
#endif
#if U_INVALID_STATE_ERROR
        case U_INVALID_STATE_ERROR:
            return make_error(env, "i18n_invalid_state_error");
#endif
#if U_COLLATOR_VERSION_MISMATCH
        case U_COLLATOR_VERSION_MISMATCH:
            return make_error(env, "i18n_collator_version_mismatch");
#endif
#if U_USELESS_COLLATOR_ERROR
        case U_USELESS_COLLATOR_ERROR:
            return make_error(env, "i18n_useless_collator_error");
#endif
#if U_NO_WRITE_PERMISSION
        case U_NO_WRITE_PERMISSION:
            return make_error(env, "i18n_no_write_permission");
#endif
#if U_STANDARD_ERROR_LIMIT
        case U_STANDARD_ERROR_LIMIT:
            return make_error(env, "i18n_standard_error_limit");
#endif
#if U_BAD_VARIABLE_DEFINITION
        case U_BAD_VARIABLE_DEFINITION:
            return make_error(env, "i18n_bad_variable_definition");
#endif
#if U_PARSE_ERROR_START
        case U_PARSE_ERROR_START:
            return make_error(env, "i18n_parse_error_start");
#endif
#if U_MALFORMED_RULE
        case U_MALFORMED_RULE:
            return make_error(env, "i18n_malformed_rule");
#endif
#if U_MALFORMED_SET
        case U_MALFORMED_SET:
            return make_error(env, "i18n_malformed_set");
#endif
#if U_MALFORMED_SYMBOL_REFERENCE
        case U_MALFORMED_SYMBOL_REFERENCE:
            return make_error(env, "i18n_malformed_symbol_reference");
#endif
#if U_MALFORMED_UNICODE_ESCAPE
        case U_MALFORMED_UNICODE_ESCAPE:
            return make_error(env, "i18n_malformed_unicode_escape");
#endif
#if U_MALFORMED_VARIABLE_DEFINITION
        case U_MALFORMED_VARIABLE_DEFINITION:
            return make_error(env, "i18n_malformed_variable_definition");
#endif
#if U_MALFORMED_VARIABLE_REFERENCE
        case U_MALFORMED_VARIABLE_REFERENCE:
            return make_error(env, "i18n_malformed_variable_reference");
#endif
#if U_MISMATCHED_SEGMENT_DELIMITERS
        case U_MISMATCHED_SEGMENT_DELIMITERS:
            return make_error(env, "i18n_mismatched_segment_delimiters");
#endif
#if U_MISPLACED_ANCHOR_START
        case U_MISPLACED_ANCHOR_START:
            return make_error(env, "i18n_misplaced_anchor_start");
#endif
#if U_MISPLACED_CURSOR_OFFSET
        case U_MISPLACED_CURSOR_OFFSET:
            return make_error(env, "i18n_misplaced_cursor_offset");
#endif
#if U_MISPLACED_QUANTIFIER
        case U_MISPLACED_QUANTIFIER:
            return make_error(env, "i18n_misplaced_quantifier");
#endif
#if U_MISSING_OPERATOR
        case U_MISSING_OPERATOR:
            return make_error(env, "i18n_missing_operator");
#endif
#if U_MISSING_SEGMENT_CLOSE
        case U_MISSING_SEGMENT_CLOSE:
            return make_error(env, "i18n_missing_segment_close");
#endif
#if U_MULTIPLE_ANTE_CONTEXTS
        case U_MULTIPLE_ANTE_CONTEXTS:
            return make_error(env, "i18n_multiple_ante_contexts");
#endif
#if U_MULTIPLE_CURSORS
        case U_MULTIPLE_CURSORS:
            return make_error(env, "i18n_multiple_cursors");
#endif
#if U_MULTIPLE_POST_CONTEXTS
        case U_MULTIPLE_POST_CONTEXTS:
            return make_error(env, "i18n_multiple_post_contexts");
#endif
#if U_TRAILING_BACKSLASH
        case U_TRAILING_BACKSLASH:
            return make_error(env, "i18n_trailing_backslash");
#endif
#if U_UNDEFINED_SEGMENT_REFERENCE
        case U_UNDEFINED_SEGMENT_REFERENCE:
            return make_error(env, "i18n_undefined_segment_reference");
#endif
#if U_UNDEFINED_VARIABLE
        case U_UNDEFINED_VARIABLE:
            return make_error(env, "i18n_undefined_variable");
#endif
#if U_UNQUOTED_SPECIAL
        case U_UNQUOTED_SPECIAL:
            return make_error(env, "i18n_unquoted_special");
#endif
#if U_UNTERMINATED_QUOTE
        case U_UNTERMINATED_QUOTE:
            return make_error(env, "i18n_unterminated_quote");
#endif
#if U_RULE_MASK_ERROR
        case U_RULE_MASK_ERROR:
            return make_error(env, "i18n_rule_mask_error");
#endif
#if U_MISPLACED_COMPOUND_FILTER
        case U_MISPLACED_COMPOUND_FILTER:
            return make_error(env, "i18n_misplaced_compound_filter");
#endif
#if U_MULTIPLE_COMPOUND_FILTERS
        case U_MULTIPLE_COMPOUND_FILTERS:
            return make_error(env, "i18n_multiple_compound_filters");
#endif
#if U_INVALID_RBT_SYNTAX
        case U_INVALID_RBT_SYNTAX:
            return make_error(env, "i18n_invalid_rbt_syntax");
#endif
#if U_INVALID_PROPERTY_PATTERN
        case U_INVALID_PROPERTY_PATTERN:
            return make_error(env, "i18n_invalid_property_pattern");
#endif
#if U_MALFORMED_PRAGMA
        case U_MALFORMED_PRAGMA:
            return enif_make_atom(env, "i19n_malformed_pragma");
#endif
#if U_UNCLOSED_SEGMENT
        case U_UNCLOSED_SEGMENT:
            return make_error(env, "i18n_unclosed_segment");
#endif
#if U_ILLEGAL_CHAR_IN_SEGMENT
        case U_ILLEGAL_CHAR_IN_SEGMENT:
            return make_error(env, "i18n_illegal_char_in_segment");
#endif
#if U_VARIABLE_RANGE_EXHAUSTED
        case U_VARIABLE_RANGE_EXHAUSTED:
            return make_error(env, "i18n_variable_range_exhausted");
#endif
#if U_VARIABLE_RANGE_OVERLAP
        case U_VARIABLE_RANGE_OVERLAP:
            return make_error(env, "i18n_variable_range_overlap");
#endif
#if U_ILLEGAL_CHARACTER
        case U_ILLEGAL_CHARACTER:
            return make_error(env, "i18n_illegal_character");
#endif
#if U_INTERNAL_TRANSLITERATOR_ERROR
        case U_INTERNAL_TRANSLITERATOR_ERROR:
            return make_error(env, "i18n_internal_transliterator_error");
#endif
#if U_INVALID_ID
        case U_INVALID_ID:
            return make_error(env, "i18n_invalid_id");
#endif
#if U_INVALID_FUNCTION
        case U_INVALID_FUNCTION:
            return make_error(env, "i18n_invalid_function");
#endif
#if U_PARSE_ERROR_LIMIT
        case U_PARSE_ERROR_LIMIT:
            return make_error(env, "i18n_parse_error_limit");
#endif
#if U_UNEXPECTED_TOKEN
        case U_UNEXPECTED_TOKEN:
            return make_error(env, "i18n_unexpected_token");
#endif
#if U_FMT_PARSE_ERROR_START
        case U_FMT_PARSE_ERROR_START:
            return make_error(env, "i18n_fmt_parse_error_start");
#endif
#if U_MULTIPLE_DECIMAL_SEPARATORS
        case U_MULTIPLE_DECIMAL_SEPARATORS:
            return make_error(env, "i18n_multiple_decimal_separators");
#endif
#if U_MULTIPLE_DECIMAL_SEPERATORS
        case U_MULTIPLE_DECIMAL_SEPERATORS:
            return make_error(env, "i18n_multiple_decimal_seperators");
#endif
#if U_MULTIPLE_EXPONENTIAL_SYMBOLS
        case U_MULTIPLE_EXPONENTIAL_SYMBOLS:
            return make_error(env, "i18n_multiple_exponential_symbols");
#endif
#if U_MALFORMED_EXPONENTIAL_PATTERN
        case U_MALFORMED_EXPONENTIAL_PATTERN:
            return make_error(env, "i18n_malformed_exponential_pattern");
#endif
#if U_MULTIPLE_PERCENT_SYMBOLS
        case U_MULTIPLE_PERCENT_SYMBOLS:
            return make_error(env, "i18n_multiple_percent_symbols");
#endif
#if U_MULTIPLE_PERMILL_SYMBOLS
        case U_MULTIPLE_PERMILL_SYMBOLS:
            return make_error(env, "i18n_multiple_permill_symbols");
#endif
#if U_MULTIPLE_PAD_SPECIFIERS
        case U_MULTIPLE_PAD_SPECIFIERS:
            return make_error(env, "i18n_multiple_pad_specifiers");
#endif
#if U_PATTERN_SYNTAX_ERROR
        case U_PATTERN_SYNTAX_ERROR:
            return make_error(env, "i18n_pattern_syntax_error");
#endif
#if U_ILLEGAL_PAD_POSITION
        case U_ILLEGAL_PAD_POSITION:
            return make_error(env, "i18n_illegal_pad_position");
#endif
#if U_UNMATCHED_BRACES
        case U_UNMATCHED_BRACES:
            return make_error(env, "i18n_unmatched_braces");
#endif
#if U_UNSUPPORTED_PROPERTY
        case U_UNSUPPORTED_PROPERTY:
            return make_error(env, "i18n_unsupported_property");
#endif
#if U_UNSUPPORTED_ATTRIBUTE
        case U_UNSUPPORTED_ATTRIBUTE:
            return make_error(env, "i18n_unsupported_attribute");
#endif
#if U_ARGUMENT_TYPE_MISMATCH
        case U_ARGUMENT_TYPE_MISMATCH:
            return make_error(env, "i18n_argument_type_mismatch");
#endif
#if U_DUPLICATE_KEYWORD
        case U_DUPLICATE_KEYWORD:
            return make_error(env, "i18n_duplicate_keyword");
#endif
#if U_UNDEFINED_KEYWORD
        case U_UNDEFINED_KEYWORD:
            return make_error(env, "i18n_undefined_keyword");
#endif
#if U_DEFAULT_KEYWORD_MISSING
        case U_DEFAULT_KEYWORD_MISSING:
            return make_error(env, "i18n_default_keyword_missing");
#endif
#if U_DECIMAL_NUMBER_SYNTAX_ERROR
        case U_DECIMAL_NUMBER_SYNTAX_ERROR:
            return make_error(env, "i18n_decimal_number_syntax_error");
#endif
#if U_FORMAT_INEXACT_ERROR
        case U_FORMAT_INEXACT_ERROR:
            return make_error(env, "i18n_format_inexact_error");
#endif
#if U_FMT_PARSE_ERROR_LIMIT
        case U_FMT_PARSE_ERROR_LIMIT:
            return make_error(env, "i18n_fmt_parse_error_limit");
#endif
#if U_BRK_INTERNAL_ERROR
        case U_BRK_INTERNAL_ERROR:
            return make_error(env, "i18n_brk_internal_error");
#endif
#if U_BRK_ERROR_START
        case U_BRK_ERROR_START:
            return make_error(env, "i18n_brk_error_start");
#endif
#if U_BRK_HEX_DIGITS_EXPECTED
        case U_BRK_HEX_DIGITS_EXPECTED:
            return make_error(env, "i18n_brk_hex_digits_expected");
#endif
#if U_BRK_SEMICOLON_EXPECTED
        case U_BRK_SEMICOLON_EXPECTED:
            return make_error(env, "i18n_brk_semicolon_expected");
#endif
#if U_BRK_RULE_SYNTAX
        case U_BRK_RULE_SYNTAX:
            return make_error(env, "i18n_brk_rule_syntax");
#endif
#if U_BRK_UNCLOSED_SET
        case U_BRK_UNCLOSED_SET:
            return make_error(env, "i18n_brk_unclosed_set");
#endif
#if U_BRK_ASSIGN_ERROR
        case U_BRK_ASSIGN_ERROR:
            return make_error(env, "i18n_brk_assign_error");
#endif
#if U_BRK_VARIABLE_REDFINITION
        case U_BRK_VARIABLE_REDFINITION:
            return make_error(env, "i18n_brk_variable_redfinition");
#endif
#if U_BRK_MISMATCHED_PAREN
        case U_BRK_MISMATCHED_PAREN:
            return make_error(env, "i18n_brk_mismatched_paren");
#endif
#if U_BRK_NEW_LINE_IN_QUOTED_STRING
        case U_BRK_NEW_LINE_IN_QUOTED_STRING:
            return make_error(env, "i18n_brk_new_line_in_quoted_string");
#endif
#if U_BRK_UNDEFINED_VARIABLE
        case U_BRK_UNDEFINED_VARIABLE:
            return make_error(env, "i18n_brk_undefined_variable");
#endif
#if U_BRK_INIT_ERROR
        case U_BRK_INIT_ERROR:
            return make_error(env, "i18n_brk_init_error");
#endif
#if U_BRK_RULE_EMPTY_SET
        case U_BRK_RULE_EMPTY_SET:
            return make_error(env, "i18n_brk_rule_empty_set");
#endif
#if U_BRK_UNRECOGNIZED_OPTION
        case U_BRK_UNRECOGNIZED_OPTION:
            return make_error(env, "i18n_brk_unrecognized_option");
#endif
#if U_BRK_MALFORMED_RULE_TAG
        case U_BRK_MALFORMED_RULE_TAG:
            return make_error(env, "i18n_brk_malformed_rule_tag");
#endif
#if U_BRK_ERROR_LIMIT
        case U_BRK_ERROR_LIMIT:
            return make_error(env, "i18n_brk_error_limit");
#endif
#if U_REGEX_INTERNAL_ERROR
        case U_REGEX_INTERNAL_ERROR:
            return make_error(env, "i18n_regex_internal_error");
#endif
#if U_REGEX_ERROR_START
        case U_REGEX_ERROR_START:
            return make_error(env, "i18n_regex_error_start");
#endif
#if U_REGEX_RULE_SYNTAX
        case U_REGEX_RULE_SYNTAX:
            return make_error(env, "i18n_regex_rule_syntax");
#endif
#if U_REGEX_INVALID_STATE
        case U_REGEX_INVALID_STATE:
            return make_error(env, "i18n_regex_invalid_state");
#endif
#if U_REGEX_BAD_ESCAPE_SEQUENCE
        case U_REGEX_BAD_ESCAPE_SEQUENCE:
            return make_error(env, "i18n_regex_bad_escape_sequence");
#endif
#if U_REGEX_PROPERTY_SYNTAX
        case U_REGEX_PROPERTY_SYNTAX:
            return make_error(env, "i18n_regex_property_syntax");
#endif
#if U_REGEX_UNIMPLEMENTED
        case U_REGEX_UNIMPLEMENTED:
            return make_error(env, "i18n_regex_unimplemented");
#endif
#if U_REGEX_MISMATCHED_PAREN
        case U_REGEX_MISMATCHED_PAREN:
            return make_error(env, "i18n_regex_mismatched_paren");
#endif
#if U_REGEX_NUMBER_TOO_BIG
        case U_REGEX_NUMBER_TOO_BIG:
            return make_error(env, "i18n_regex_number_too_big");
#endif
#if U_REGEX_BAD_INTERVAL
        case U_REGEX_BAD_INTERVAL:
            return make_error(env, "i18n_regex_bad_interval");
#endif
#if U_REGEX_MAX_LT_MIN
        case U_REGEX_MAX_LT_MIN:
            return make_error(env, "i18n_regex_max_lt_min");
#endif
#if U_REGEX_INVALID_BACK_REF
        case U_REGEX_INVALID_BACK_REF:
            return make_error(env, "i18n_regex_invalid_back_ref");
#endif
#if U_REGEX_INVALID_FLAG
        case U_REGEX_INVALID_FLAG:
            return make_error(env, "i18n_regex_invalid_flag");
#endif
#if U_REGEX_LOOK_BEHIND_LIMIT
        case U_REGEX_LOOK_BEHIND_LIMIT:
            return make_error(env, "i18n_regex_look_behind_limit");
#endif
#if U_REGEX_SET_CONTAINS_STRING
        case U_REGEX_SET_CONTAINS_STRING:
            return make_error(env, "i18n_regex_set_contains_string");
#endif
#if U_REGEX_OCTAL_TOO_BIG
        case U_REGEX_OCTAL_TOO_BIG:
            return make_error(env, "i18n_regex_octal_too_big");
#endif
#if U_REGEX_MISSING_CLOSE_BRACKET
        case U_REGEX_MISSING_CLOSE_BRACKET:
            return make_error(env, "i18n_regex_missing_close_bracket");
#endif
#if U_REGEX_INVALID_RANGE
        case U_REGEX_INVALID_RANGE:
            return make_error(env, "i18n_regex_invalid_range");
#endif
#if U_REGEX_STACK_OVERFLOW
        case U_REGEX_STACK_OVERFLOW:
            return make_error(env, "i18n_regex_stack_overflow");
#endif
#if U_REGEX_TIME_OUT
        case U_REGEX_TIME_OUT:
            return make_error(env, "i18n_regex_time_out");
#endif
#if U_REGEX_STOPPED_BY_CALLER
        case U_REGEX_STOPPED_BY_CALLER:
            return make_error(env, "i18n_regex_stopped_by_caller");
#endif
#if U_REGEX_ERROR_LIMIT
        case U_REGEX_ERROR_LIMIT:
            return make_error(env, "i18n_regex_error_limit");
#endif
#if U_PLUGIN_ERROR_START
        case U_PLUGIN_ERROR_START:
            return make_error(env, "i18n_plugin_error_start");
#endif
#if U_PLUGIN_TOO_HIGH
        case U_PLUGIN_TOO_HIGH:
            return make_error(env, "i18n_plugin_too_high");
#endif
#if U_PLUGIN_DIDNT_SET_LEVEL
        case U_PLUGIN_DIDNT_SET_LEVEL:
            return make_error(env, "i18n_plugin_didnt_set_level");
#endif
#if U_PLUGIN_ERROR_LIMIT
        case U_PLUGIN_ERROR_LIMIT:
            return make_error(env, "i18n_plugin_error_limit");
#endif
        default:
            return make_error(env, "i18n_unknown");
    }
}





inline ERL_NIF_TERM enum_to_term(ErlNifEnv* env, UEnumeration* en) {
    
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
            return make_error(env, "i18n_enum_elem_too_long");

        head = enif_make_atom(env, buf);
        tail = enif_make_list_cell(env, head, tail);
    }
}



inline ERL_NIF_TERM string_to_term(ErlNifEnv* env, const UnicodeString& s) {
        ERL_NIF_TERM term;
        size_t len;
        const UChar* buf;
        unsigned char* bin;

        /* length in bytes */
        len = FROM_ULEN(s.length());
        buf = s.getBuffer();
        bin = enif_make_new_binary(env, len, &term);
        memcpy(bin, (const char*) buf, len);

        return term;
}


inline UnicodeString copy_binary_to_string(const ErlNifBinary& in) {
    /* Readonly-aliasing UChar* constructor. */
    return UnicodeString(
        (const UChar*) in.data,
        TO_ULEN(in.size));
}

inline UnicodeString binary_to_string(const ErlNifBinary& in) {
    /* Readonly-aliasing UChar* constructor. */
    return UnicodeString(
        false,
        (const UChar*) in.data,
        TO_ULEN(in.size));
}

#if I18N_DATE
inline ERL_NIF_TERM calendar_to_double(ErlNifEnv* env, const UCalendar* cal) {
    UDate date;
    UErrorCode status = U_ZERO_ERROR;

    date = ucal_getMillis(cal, &status);
    CHECK(env, status);

    return enif_make_double(env, (double) date);
}
#endif

inline static ERL_NIF_TERM bool_to_term(UBool value) {
    return value ? ATOM_TRUE : ATOM_FALSE; 
}



typedef const char* (*avail_fun)(int32_t);


static ERL_NIF_TERM generate_available(ErlNifEnv* env, avail_fun fun, 
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
void iterator_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
void iterator_close(char* obj) 
{
    if (obj != NULL)
        ubrk_close((UBreakIterator*) obj);   
}
char* iterator_clone(char* obj) 
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

int iterator_open(UBreakIterator * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &iterator_clone, &iterator_close);
} 








/**
 * Helpers
 */
int parseIteratorType(const char * type) 
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
static ERL_NIF_TERM from_utf8(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
static ERL_NIF_TERM to_utf8(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

static ERL_NIF_TERM to_nfc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return do_norm(env, in, nfc_normalizer);
}

static ERL_NIF_TERM to_nfd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return do_norm(env, in, nfd_normalizer);
}

static ERL_NIF_TERM to_nfkc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[0], &in)) 
        return enif_make_badarg(env);

    return do_norm(env, in, nfkc_normalizer);
}

static ERL_NIF_TERM to_nfkd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

static ERL_NIF_TERM to_upper(ErlNifEnv* env, int argc, 
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
static ERL_NIF_TERM to_lower(ErlNifEnv* env, int argc, 
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
static ERL_NIF_TERM to_title(ErlNifEnv* env, int argc, 
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
static ERL_NIF_TERM len(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
static ERL_NIF_TERM split(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
static ERL_NIF_TERM split_index(ErlNifEnv* env, int argc, 
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
static ERL_NIF_TERM get_iterator(ErlNifEnv* env, int argc, 
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



static ERL_NIF_TERM iterator_locales(ErlNifEnv* env, int argc, const
    ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return generate_available(env, ubrk_getAvailable, ubrk_countAvailable());
}


static int i18n_string_load(ErlNifEnv *env, void ** /*priv_data*/, 
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



































#if I18N_COLLATION
UCollator* base_col;
static ErlNifResourceType* collator_type = 0;



/* Called from erl_nif. */
void collator_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
void collator_close(char* obj) 
{ 
    if (obj != NULL)
        ucol_close((UCollator*) obj);
}
char* collator_clone(char* obj) 
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

int collator_open(UCollator * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &collator_clone, &collator_close);
} 





int parseAttrValue(const char * type) 
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
int parseAttrKey(const char * type) 
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

int do_iterator_options(ErlNifEnv* env, UCollator* col, 
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
static ERL_NIF_TERM get_collator(ErlNifEnv* env, int argc, 
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
            return list_element_error(env, argv[1], index);
        }
    }

    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}
inline void do_sort_key(
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
static ERL_NIF_TERM sort_key(ErlNifEnv* env, int argc, 
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
    CHECK_DEST(env, status, 
        enif_release_binary(&out);
    );
    return enif_make_binary(env, &out);
}
static ERL_NIF_TERM compare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, in2;
    UCollator* col;
    cloner* ptr;
    UCollationResult res;

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
            return ATOM_EQUAL;
            break;
            
        case UCOL_GREATER: 
            return ATOM_GREATER;
            break;

        case UCOL_LESS:
            return ATOM_LESS;
            break;
    }
    ERROR(env, U_INTERNAL_PROGRAM_ERROR);
}











static int i18n_collation_load(ErlNifEnv *env, void ** /*priv_data*/, 
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
    
    return 0;
}

static ERL_NIF_TERM collator_locales(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return generate_available(env, ucol_getAvailable, ucol_countAvailable());
}

static void i18n_collation_unload(ErlNifEnv* /*env*/, void* /*priv*/)
{
    ucol_close(base_col);
    return;
}
#endif


























#if I18N_SEARCH


/* isearch_common_store: Stores const values for each copy of searcher */
static ErlNifResourceType* isearch_common_type = 0;

#if I18N_INFO
int isearch_count = 0;
ErlNifMutex* isearch_count_mtx;
int isearch_common_count = 0;
ErlNifMutex* isearch_common_count_mtx;
#endif

/* Stores default text */
static UChar isearch_text[1];
static int32_t isearch_text_len = 1;

typedef struct {
    ErlNifEnv* env;
    ERL_NIF_TERM pattern;
    ERL_NIF_TERM col;
} ISearchCommon;


inline ISearchCommon* isearch_common_alloc() 
{
#if I18N_INFO
    enif_mutex_lock(isearch_common_count_mtx);
    isearch_common_count++;
    enif_mutex_unlock(isearch_common_count_mtx);
#endif

    return (ISearchCommon*) enif_alloc_resource(isearch_common_type, 
                sizeof(ISearchCommon));
}

void isearch_common_dtor(ErlNifEnv* /*env*/, void* obj) 
{
#if I18N_INFO
    enif_mutex_lock(isearch_common_count_mtx);
    isearch_common_count--;
    enif_mutex_unlock(isearch_common_count_mtx);
#endif

    enif_free_env(((ISearchCommon*) obj)->env);
}


ISearchCommon* isearch_common_open(const ERL_NIF_TERM& pattern, 
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
UStringSearch* isc_to_uss(ISearchCommon* isc)
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

void isearch_close(ISearch* is)
{
    usearch_close(is->ss);
    /* important: free the common resourse for this clone */
    enif_release_resource(is->isc);
    enif_free(is);
}

ISearch* isearch_clone(ISearch* is1)
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

ISearch* isearch_open(const ERL_NIF_TERM& col,
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

UStringSearch* isearch_get(const ISearch* is)
{
    if ((is == NULL) || (is->ss == NULL))
        return NULL;
    return is->ss;
}









/* This constraction is same for all parts of this file. */
static ErlNifResourceType* searcher_type = 0;



/* Called from erl_nif. */
void searcher_dtor(ErlNifEnv* /*env*/, void* obj) 
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
void searcher_close(char* obj) 
{ 
    if (obj != NULL)
        isearch_close((ISearch*) obj);
}
char* searcher_clone(char* obj) 
{
    return (char*) isearch_clone((ISearch*) obj);
}

int searcher_open(ISearch * obj, cloner* c)
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


static ERL_NIF_TERM search_open(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM search_index(ErlNifEnv* env, int argc, 
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
static ERL_NIF_TERM search_match_all(ErlNifEnv* env, int argc, 
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
static ERL_NIF_TERM search_match(ErlNifEnv* env, int argc, 
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
        res = ATOM_FALSE;
    }

    return res;
}
static ERL_NIF_TERM search_test(ErlNifEnv* env, int argc, 
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

    return bool_to_term(pos != USEARCH_DONE);
}

#if I18N_INFO
static ERL_NIF_TERM i18n_search_info(ErlNifEnv *env)
{
    return  enif_make_list1(env, 
                enif_make_tuple2(env,
                    ATOM_RESOURCE,
                    enif_make_list2(env,
                        enif_make_tuple2(env,
                            enif_make_atom(env, (const char*) "isearch"),
                                enif_make_list1(env, 
                                    enif_make_tuple2(env,
                                        ATOM_COUNT,
                                        enif_make_int(env, isearch_count)))),
                        enif_make_tuple2(env,
                            enif_make_atom(env, (const char*) "isearch_common"),
                                enif_make_list1(env, 
                                    enif_make_tuple2(env,
                                        ATOM_COUNT,
                                        enif_make_int(env,
                                            isearch_common_count))))
            )));
}
#endif

static int i18n_search_load(ErlNifEnv *env, void ** /*priv_data*/, 
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


























#if I18N_MESSAGE


U_NAMESPACE_BEGIN
/**
 * This class isolates our access to private internal methods of
 * MessageFormat.  It is never instantiated; it exists only for C++
 * access management.
 */
class MessageFormatAdapter {
public:
    static const Formattable::Type* getArgTypeList(const MessageFormat& m,
                                                   int32_t& count);
};
const Formattable::Type*
MessageFormatAdapter::getArgTypeList(const MessageFormat& m,
                                     int32_t& count) {
    return m.getArgTypeList(count);
}
U_NAMESPACE_END

U_NAMESPACE_USE





static ErlNifResourceType* message_type = 0;


/* Called from erl_nif. */
void message_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
void message_close(char* obj) 
{ 
    if (obj != NULL)
        umsg_close((UMessageFormat*) obj);
}
char* message_clone(char* obj) 
{
    UErrorCode status = U_ZERO_ERROR;

    obj = (char*) umsg_clone(
        (UMessageFormat*) obj,
        &status 
    );
    if(U_FAILURE(status)) { 
        return NULL;
    } 
    return obj;
}

int message_open(UMessageFormat * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &message_clone, &message_close);
} 








/**
 * NIFs
 */

/* Get a message format 
 * i18n:to(i18n_message:format(i18n_message:open(i18n:from("{0,date}")),
 * [{i18n:from("0"), i18n_nif:date_now()}])).  
 * <<"2011 9 28">> */

static ERL_NIF_TERM open_format(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out;
    ErlNifBinary in;
    char locale[LOCALE_LEN];
    UErrorCode status = U_ZERO_ERROR;
    UMessageFormat* msg;
    cloner* res;

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
            NULL, 
            &status);
    CHECK(env, status);


    res = (cloner*) enif_alloc_resource(message_type, sizeof(cloner));
    if (message_open(msg, res)) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }
    out = enif_make_resource(env, res);
    enif_release_resource(res);
    /* resource now only owned by "Erlang" */
    return out;
}
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

inline void append_atom(char * atom, UnicodeString& s)
{
    while (*atom) {
        s.append((UChar) *atom);
        atom++;
    } 
}

static ERL_NIF_TERM format(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in, name;
    ERL_NIF_TERM out, list, argt;
    int32_t len, mcount; 
    cloner* ptr;
    unsigned int count, i;
    UErrorCode status = U_ZERO_ERROR;
    UnicodeString appendTo;
    const MessageFormat* fmt;
    const Formattable::Type* types;

    ERL_NIF_TERM* tuple;
    double tDouble;
    int tInt;
    ErlNifSInt64 tInt64;

    if ((argc != 2) && (argc != 3))
        return enif_make_badarg(env);

    if(!(enif_get_list_length(env, argv[1], &count)
      && enif_get_resource(env, argv[0], message_type, (void**) &ptr))) {
        return enif_make_badarg(env);
    }

    fmt = (const MessageFormat*) cloner_get(ptr);
    CHECK_RES(env, fmt);


    if (argc == 3) {
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
    UnicodeString* names = new UnicodeString[count ? count : 1];

    i = 0;
    list = argv[1];

    /* TODO: dirty, but stable
     * If we use c API we cannot check args.
     * If we use c++ API we cannot get requered type.
     * So we will use c++ API for c implementation (private API). */
    types = MessageFormatAdapter::getArgTypeList(*fmt, mcount);

    if (mcount < (int32_t) count) 
        return enif_make_badarg(env); /* too few elements in the list */

    while (enif_get_list_cell(env, list, &out, &list)) {

        if (enif_get_tuple(env, out, &len, (const ERL_NIF_TERM**) &tuple)
            && (len == 2)) { 
            /* [..., {Id, Arg}, ...] */

            if (enif_inspect_binary(env, tuple[0], &name)) {
                /* typeof(Id) == unicode_string */
                names[i].append((UChar*) name.data, 
                    (int32_t) TO_ULEN(name.size));
            } else 

            if (enif_get_int(env, tuple[0], &tInt)) {
                /* typeof(Id) == integer */
                append_uint(tInt, names[i]);
            } else 

            if (enif_is_atom(env, tuple[0])) {
                /* typeof(Id) == atom */
                char atom[ATOM_LEN];
                if (!enif_get_atom(env, tuple[0], (char*) atom, ATOM_LEN,
                    ERL_NIF_LATIN1))
                    goto bad_elem;
                
                append_atom((char *) atom, names[i]);
                    
            } else 
                goto bad_elem;

                
            argt = tuple[1];
        } else {
            /* [..., Arg, ...] */
            argt = out;
            append_uint((unsigned int) i, names[i]);
        }


        /* out is a head
           len is arity
           reuse name variable as an argument */
        switch (types[i]) {
            
            case Formattable::kDate:
 
                if (!enif_get_double(env, argt, &tDouble)) 
                    goto bad_elem;

                args[i].setDate((UDate) tDouble);
                break;
            
            case Formattable::kDouble:
 
                if (!enif_get_double(env, argt, &tDouble)) 
                    goto bad_elem;

                args[i].setDouble(tDouble);
                break;
            
            case Formattable::kLong:
 
                if (!enif_get_int(env, argt, &tInt))
                    goto bad_elem;

                args[i].setLong((int32_t) tInt);
                break;
 
            case Formattable::kInt64:

                if (!enif_get_int64(env, argt, &tInt64)) 
                    goto bad_elem;

                args[i].setInt64((int64_t) tInt64);
                break;
                
            case Formattable::kString:

                if (!enif_inspect_binary(env, argt, &name)) 
                    goto bad_elem;

                args[i].setString(
                    * new UnicodeString(
                        (const UChar*) name.data, 
                        (int32_t) TO_ULEN(name.size)));
                break;
 
           default:
                goto bad_elem;
        }
        
        i++;
    }
    
    fmt->format(
        (const UnicodeString *) names,
        (const Formattable *) args,
        (int32_t) count,
        appendTo,
        status      
    );
    delete[] args;
    delete[] names;

    CHECK(env, status);

    return string_to_term(env, appendTo);

    bad_elem:
        /* Memory deallocation */
        delete[] args;
        delete[] names;
        return list_element_error(env, argv[1], i);
}







static int i18n_message_load(ErlNifEnv *env, void ** /*priv_data*/, 
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






































#if I18N_REGEX

static ErlNifResourceType* regex_type = 0;


/* Called from erl_nif. */
void regex_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
void regex_close(char* obj) 
{ 
    if (obj != NULL)
        delete (RegexPattern*) obj;
}
char* regex_clone(char* obj) 
{
    obj = (char*) new RegexPattern(
        * ((RegexPattern*) obj)
    );
    return obj;
}

int regex_open(RegexPattern * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &regex_clone, &regex_close);
} 








/**
 * NIFs
 */

/* Get a message format  */
static ERL_NIF_TERM open_regex(ErlNifEnv* env, int argc, 
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
        return parse_error(env, "i18n_regex_error", &pe);
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
static ERL_NIF_TERM regex_replace_all(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM regex_replace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
static ERL_NIF_TERM regex_split(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM regex_test(ErlNifEnv* env, int argc, 
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

    return bool_to_term(res);
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

static ERL_NIF_TERM regex_match_all(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM regex_match(ErlNifEnv* env, int argc, 
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

static int i18n_regex_load(ErlNifEnv *env, void ** /*priv_data*/, 
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











#if I18N_LOCALE
static ERL_NIF_TERM locale_name(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;

    int32_t /*value_len,*/     key_len; 
    char    value[LOCALE_LEN], key[LOCALE_LEN];

    if (argc != 1)
        return enif_make_badarg(env);

    key_len = enif_get_atom(env, argv[0], (char*) key, 
                    LOCALE_LEN, ERL_NIF_LATIN1);

    if (!key_len) {
        return enif_make_badarg(env);
    }

    
    /*value_len =*/ uloc_getName((const char*) key, /* Locale Id */
        (char *)  value, /* Name */
        (int32_t) LOCALE_LEN,
        &status);
    CHECK(env, status);

    return enif_make_atom(env, value);
}
static ERL_NIF_TERM locale_parent(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;

    int32_t /*value_len,*/     key_len; 
    char    value[LOCALE_LEN], key[LOCALE_LEN];

    if (argc != 1)
        return enif_make_badarg(env);

    key_len = enif_get_atom(env, argv[0], (char*) key, 
                    LOCALE_LEN, ERL_NIF_LATIN1);

    if (!key_len) {
        return enif_make_badarg(env);
    }

    
    /*value_len =*/ uloc_getParent((const char*) key, /* Locale Id */
        (char *)  value, /* Name */
        (int32_t) LOCALE_LEN,
        &status);
    CHECK(env, status);

    return enif_make_atom(env, value);
}
static ERL_NIF_TERM locale_language_tag(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;

    int32_t /*value_len,*/     key_len; 
    char    value[LOCALE_LEN], key[LOCALE_LEN];

    if (argc != 1)
        return enif_make_badarg(env);

    key_len = enif_get_atom(env, argv[0], (char*) key, 
                    LOCALE_LEN, ERL_NIF_LATIN1);

    if (!key_len) {
        return enif_make_badarg(env);
    }

    
    /*value_len =*/ uloc_toLanguageTag((const char*) key, /* Locale Id */
        (char *)  value, /* Name */
        (int32_t) LOCALE_LEN,
        FALSE,
        &status);
    CHECK(env, status);

    return enif_make_atom(env, value);
}
static ERL_NIF_TERM locale_base_name(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM argv[])
{
    UErrorCode status = U_ZERO_ERROR;

    int32_t /*value_len,*/     key_len; 
    char    value[LOCALE_LEN], key[LOCALE_LEN];

    if (argc != 1)
        return enif_make_badarg(env);

    key_len = enif_get_atom(env, argv[0], (char*) key, 
                    LOCALE_LEN, ERL_NIF_LATIN1);

    if (!key_len) {
        return enif_make_badarg(env);
    }

    
    /*value_len =*/ uloc_getBaseName((const char*) key, /* Locale Id */
        (char *)  value, /* name */
        (int32_t) LOCALE_LEN,
        &status);
    CHECK(env, status);

    return enif_make_atom(env, value);
}

#endif


#if I18N_DATE

static ErlNifResourceType* calendar_type = 0;


/* Called from erl_nif. */
void calendar_dtor(ErlNifEnv* /*env*/, void* obj) 
{
    /* Free memory */
    cloner_destroy((cloner*) obj); 
}



/* Called from cloner for each thread. */
void calendar_close(char* obj) 
{ 
    if (obj != NULL)
        ucal_close((UCalendar*) obj);
}
char* calendar_clone(char* obj) 
{
    UErrorCode status = U_ZERO_ERROR;
    
    obj = (char*) ucal_clone((UCalendar*) obj, &status);
    if(U_FAILURE(status)) { 
        return NULL;
    } 

    return obj;
}

int calendar_open(UCalendar * obj, cloner* c)
{
    return cloner_open((char *) obj, c, &calendar_clone, &calendar_close);
} 







/**
 * NIFs
 */


inline UCalendarType parserCalendarType(const char * type) 
{
    return (!strcmp((char*) "gregorian",   type)) ? UCAL_GREGORIAN :
           (!strcmp((char*) "traditional", type)) ? UCAL_TRADITIONAL :
            UCAL_DEFAULT;
}
static ERL_NIF_TERM open_calendar(ErlNifEnv* env, int argc, 
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

int parseCalendarDateField(const char * type) 
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

void do_ucal_set(UCalendar * cal,
        UCalendarDateFields field,
        int32_t amount,
        UErrorCode * /*status*/) {
    if (field == UCAL_MONTH)
        amount--; /* month from 0 */
    ucal_set(cal, field, amount);
} 

static ERL_NIF_TERM date_set(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM date_add(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM date_roll(ErlNifEnv* env, int argc, 
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


static ERL_NIF_TERM date_clear(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM date_get_field(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM date_get_fields(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM date_now(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return enif_make_double(env, (double) ucal_getNow());
}

static ERL_NIF_TERM date_is_weekend(ErlNifEnv* env, int argc, 
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


static ERL_NIF_TERM date_get3(ErlNifEnv* env, int argc, 
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

static ERL_NIF_TERM date_get6(ErlNifEnv* env, int argc, 
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


static ERL_NIF_TERM calendar_locales(ErlNifEnv* env, int argc, 
    const ERL_NIF_TERM /*argv*/[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return generate_available(env, ucal_getAvailable, 
            ucal_countAvailable());
}

static int i18n_date_load(ErlNifEnv *env, void ** /*priv_data*/, 
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


