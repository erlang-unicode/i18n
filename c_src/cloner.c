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
 *  @author Michael Uvarov <freeakk@gmail.com>
 *  =====================================================================
 */

#include "erl_nif.h"
#include "cloner.h"
#include <stdlib.h>

int cloner_open(char * obj,
    cloner* c,
    ptr2clonefn clone_fn,
    ptr2destrfn destr_fn) 
{
    struct cloner_element* e;

    c->mutex = enif_mutex_create((char*) "Cloner mutex.");
    c->clone_fn = clone_fn;
    c->destr_fn = destr_fn;
    c->count = 1;

    
    c->array = enif_alloc(sizeof(struct cloner_array));
    if (c->array == NULL) {
        return 1;
    }
    c->array->next = NULL;

    e = c->array->elems;
    e->data = obj;
    e->tid = enif_thread_self();

    return 0;
}

char* cloner_get(cloner* c) 
{
    size_t i, ii, count;
    ErlNifTid self;
    struct cloner_array* a;
    struct cloner_element* e;
    char* ptr;

    count = c->count;
    a = c->array;
    self = enif_thread_self();
    
    /* readonly: try to find a copy for this thread */
    for(i=0, ii=0; i<count; i++, ii++) 
    {
        if (ii == CLONER_MAX) {
            ii = 0;
            a = a->next;
        }

        if (enif_equal_tids(self, (a->elems[ii].tid))) {
            return a->elems[ii].data;
        }
    }


    
    /* cloner function has its own mutex. */
    ptr = (*c->clone_fn)(c->array->elems->data);

    /* write */
    enif_mutex_lock(c->mutex);
    count = c->count + 1;

    /* i = old_count+1 */
    /* count = new_count+1 */
    /* new_count>old_count */
    /* old_count<new_count */
    /* old_count+1<new_count+1 */
    /* i<count */
    if (i < count) {
        for(; a->next != NULL; a = a->next) {}
    }
    ii = div(c->count, CLONER_MAX).rem;
    if (!ii) {
        a->next = enif_alloc(sizeof(struct cloner_array));
        a = a->next;
        a->next = NULL;
    }
    e = &(a->elems[ii]);
    e->tid = self;
    e->data = ptr;
    c->count = count;
    enif_mutex_unlock(c->mutex);
    return e->data;
}

void cloner_destroy(cloner* c) 
{
    size_t i, ii;
    struct cloner_array* a, *next;
    enif_mutex_destroy(c->mutex);
    a = c->array;
    
    for(i=c->count, ii=0; i; i--, ii++) 
    {
        if (ii == CLONER_MAX) {
            next = a->next;
            enif_free(a);
            a = next;
            ii = 0;
        }
        (*c->destr_fn) ((a->elems[ii]).data);
    }
    enif_free(a);
}

