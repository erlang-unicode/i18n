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


#if I18N_DATE
    NIF_EXPORT(date_now)
    NIF_EXPORT(open_calendar)
    NIF_EXPORT(date_set)
    NIF_EXPORT(date_add)
    NIF_EXPORT(date_roll)
    NIF_EXPORT(date_clear)
    NIF_EXPORT(date_is_weekend)
    NIF_EXPORT(date_get3)
    NIF_EXPORT(date_get6)
    NIF_EXPORT(date_get_field)
    NIF_EXPORT(date_get_fields)
    NIF_EXPORT(calendar_locales)

int i18n_date_load(ErlNifEnv *, void **, ERL_NIF_TERM);

#endif
