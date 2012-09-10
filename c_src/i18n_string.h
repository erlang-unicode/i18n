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


/**
 * i18n_string
 */
#if I18N_STRING

NIF_EXPORT(from_utf8)
NIF_EXPORT(to_utf8)
NIF_EXPORT(endian)
NIF_EXPORT(to_nfc)
NIF_EXPORT(to_nfd)
NIF_EXPORT(to_nfkc)
NIF_EXPORT(to_nfkd)
NIF_EXPORT(to_upper)
NIF_EXPORT(to_lower)
NIF_EXPORT(to_title)
NIF_EXPORT(len)
NIF_EXPORT(split)
NIF_EXPORT(split)
NIF_EXPORT(split_index)
NIF_EXPORT(get_iterator)
NIF_EXPORT(iterator_locales)
NIF_EXPORT(case_compare)
NIF_EXPORT(non_case_compare)


int i18n_string_load(ErlNifEnv *, void **, 
    ERL_NIF_TERM);
void i18n_string_unload(ErlNifEnv*, void*);
#endif
