/* vim: set filetype=c shiftwidth=4 tabstop=4 expandtab tw=80: */

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

#define CLONER_MAX 10

typedef char* (*ptr2clonefn)(char*);
typedef void  (*ptr2destrfn)(char*);

struct cloner_element {
    ErlNifTid tid;
    char * data;
};
struct cloner_store {
    size_t size; /* in bytes */
    ErlNifMutex* mutex;
    ptr2clonefn clone_fn;
    ptr2destrfn destr_fn;
    size_t count; /* in elements */
    struct cloner_array* array;
};
struct cloner_array {
    struct cloner_array* next;
    struct cloner_element elems[CLONER_MAX];
};
typedef struct cloner_store cloner;

int cloner_open(char * obj,
    cloner* c,
    ptr2clonefn clone_fn,
    ptr2destrfn destr_fn);
char* cloner_get(cloner* c);
void cloner_destroy(cloner* c);
