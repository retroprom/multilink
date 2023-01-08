/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is Multilink.
 *
 * The Initial Developer of the Original Code is AvantGo, Inc.
 * Portions created by AvantGo, Inc. are Copyright (C) 1998-1999
 * AvantGo, Inc. All Rights Reserved.
 *
 * multilink.c
 *
 * created: David Williams, djw@avantgo.com, July 15, 1998.
 *
 * Based in part on:
 *   obj-res, by Dionne & Associates
 *   Gcc PalmOS runtime support by Kresten Krab Thorup 
 *   gtscc by David Williams (see www.mozilla.org)
 *
 * Multilink makes it possible to link large, multiple code segment
 * PalmOS applications. It can also create multiple code segment PalmOS
 * SysLib*() compatible libraries.
 *
 * Multilink is a linker driver program. It sets up all the link objects,
 * application data, and inter segment call glue, but relies on gcc to do
 * the actual work of linking. Multilink is non-invasive to the gcc
 * compile phase. That is, no special code is generated for an
 * inter-segment call. Gcc is used to fully link each code segment, and
 * so is called at least N times for one multilink, where N is the number
 * of code segments.
 * 
 * Cross segment calls are achieved by having a local proxy (or
 * surrogate) function for all "outside" segment functions. The assembler
 * generated surrogate function keeps the local arguments intact, and
 * makes the cross segment jump to the "real" function via a simple
 * indexed jump table that exists in each segment. The jump tables are
 * generated in the code segment. There is a small table of these jump
 * tables, and this is allocated in system heap memory. A pointer to this
 * table of jump tables is kept in the global data segment. In addition a
 * pointer to the table of jmp tables is kept in a PalmOS Ftr, so that a
 * cross segment call can be made even when no globals are present.
 * 
 * Multilink generates one data segment that is shared by all code
 * segments. This is done via extreme linker gymnastics. Multilink
 * analyses all the source objects, keeps track of all data references,
 * and then feeds the linker a series of dummy data with each code
 * segment. The dummy data is added before and after the real data of the
 * code segment and serves the purpose of "getting all the offsets" to
 * match. That is, all global references to (say) Foo use the same a4
 * relative offset. In addition, initialized data from each code segment
 * is merged into the same final data segment.
 * 
 * Multilink replaces both gcc as linker, and obj-res in the traditional
 * single segment gcc build process. Multilink actually calls obj-res for
 * each code segment, then merges the results for global data, and
 * renames everything else according to the basename argument - djw.
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <arpa/inet.h>

#include <bfd.h>

#include "multilinkrloc.h"

#define MULTILINK_VERSION       0x00000300L /* 00MMmmuu = MM.mm[.uu] */
/*
 * Version history.
 * 00000300 Release version number. .coff to .out, removed need for -basename.
 * 00000213 Documentation cleanup.
 * 00000212 Fixed 2.11 for 0.5 tool chain (needed to set A4 from A5).
 * 00000211 In startup (crt0): Preserve app globals in sub calls.
 * 00000210 Reimplemented support standard libraries to make them work
 *          just like user libraries.
 * 0000020F Added support for stdlib globals.
 * 0000020E Added hack support for -Ldir.
 * 0000020D Added support for libraries (lib.a).
 * 0000020C Added code that links stdlibs into one only segment. Changed
 *          semantics of -stdlib, -nostdlib. Added -lgcc, -lc.
 * 0000020B Added -gdb-script option. Fixed POSE complaint in crt0.c.
 * 0000020A New "a5 always on" runtime model for a5 mode. Patches
 *          to gdb startup code for a5 world.
 * 00000209 Windows patches integration.
 * 00000208 Reference count the code jmptables.
 * 00000207 Added tool side patching of the section vma addresses.
 * 00000206 Support for gcc 2.95 (egcs) added. Lots rewritten. What was
 *          handled by obj-res is now handled inline. Simplified loader
 *          relocation code (a little).
 * 00000205 Bug fixes: a5 was be incorectly set the same as a4.
 *                     Moved the app info block space from *a4 to *a5.
 * 00000204 Bug fixes: unresolved references to global data.
 *                     filename before <segment> in map file crasher.
 *                     object file > segmentmaxsize crasher.
 * 00000203 PalmOS 3.3 fix: open/close the library database in shlibcrt0.c.
 * 00000202 Removed bogus exit when (a4 == 0) in crt0.c.
 * 00000201 Changed LoadCodeSegments() so that ROMed code resources
 *          will not be unlocked in UnloadCodeSegments().
 * 00000200 First versioned version.
 *          Fixed handling where there is only one code segment.
 *          Added -version option.
 *          Cleaned up Makefile.
 */
#define MULTILINK_VERSION_MAJOR ((MULTILINK_VERSION >> 16) & 0xff)
#define MULTILINK_VERSION_MINOR ((MULTILINK_VERSION >> 8) & 0xff)
#define MULTILINK_VERSION_MICRO ((MULTILINK_VERSION) & 0xff)

#ifndef MULTILINK_LIBDIR
#define MULTILINK_LIBDIR "/usr/local/m68k-palmos/lib/multilib"
#endif

/* crt variants */
#define LIBFILE_CRT0    "crt/crt0.o"
#define LIBFILE_GCRT0   "crt/gcrt0.o"
#define LIBFILE_SLCRT0  "crt/slcrt0.o"
#define LIBFILE_SLGCRT0 "crt/slgcrt0.o"

/* library files */
#define LIBFILE_DATA    "lib/multilinkdata.o"
#define LIBFILE_LOAD    "lib/multilinkload.o"
#define LIBFILE_GET     "lib/multilinkget.o"
#define LIBFILE_RLOC    "lib/multilinkrloc.o"

#define DEFAULT_AMX_PRIORITY 30   /* AMX task priority */
#define DEFAULT_HEAP_SIZE    4096 /* required heap space */
#define DEFAULT_STACK_SIZE   4096 /* required stack space */

#ifdef MULTILINK_A4_GLOBALS
#define DEFAULT_A4_GLOBALS 1
#else
#define DEFAULT_A4_GLOBALS 0
#endif

#define DEFAULT_AOUT_PREFIX "a"
#define DEFAULT_AOUT_SUFFIX ".out"

#ifdef MULTILINK_RELOCATION_OLD
#define DEFAULT_RELOCATION_OLD 1
#else
#define DEFAULT_RELOCATION_OLD 0
#endif

#define MEM_NEW(t)       (t*)malloc(sizeof(t))
#define MEM_NEWZAP(t)    (t*)calloc(1, sizeof(t))
#define MEM_CALLOC(n, s) calloc((n), (s))
#define MEM_MALLOC(n)    malloc((n))
#define MEM_VECTOR(t, n) (t*)malloc(sizeof(t) * (n))
#define MEM_FREE(p)      free(p)

typedef enum TargetType
{
    TARGET_APP,
    TARGET_SYSLIB
} TargetType;

/* lifted from obj-res.c */
static unsigned char *compress_data(unsigned char *raw,
                                    long data_len,
                                    long bss_len,
                                    long *comp_len)
{
    unsigned char *dp;
    unsigned char *wp;
    int block_len;
    int count;
    int total = 0;

    wp = dp = malloc(data_len * 2 + 64);  /* data could in theory grow a lot */

    /* Thanks to Darrin Massena for this algorithm */

    *(unsigned int *)wp = htonl(data_len);
    wp += 4;
    total += 4;

    if ((data_len + bss_len) > 0x7ffc) {
        fprintf(stderr, "error: .data and .bss too large for data #0 resource\n");
        exit(1);
    }

    /* A5 offset */
    *(unsigned int *)wp = htonl(-(((data_len + bss_len) + 3) & 0x7ffc));

    wp += 4;
    total += 4;

    count = data_len;
    while(count) {

        block_len = (count < 0x80) ? count : 0x80;
        *(wp++) = (block_len - 1) | 0x80;
        total++;

        memcpy(wp, raw, block_len);

        wp +=    block_len;
        raw +=   block_len;
        total += block_len;
        count -= block_len;
    }

    /* 3 separator bytes, and 2 more A5 offsets, all 0, = 11 more bytes */

    memset(wp, 0, 11);
    total += 11;
    wp += 11; /* I think this should be here */

    /* 6 longs of 0 for future compatibility with MW relocation extensions */

    memset(wp, 0, 6*4);
    total += 6*4;

    *comp_len = total;
    return dp;
}

typedef struct List
{
	struct List* _next;
	void*        thing;
} List;

unsigned
ListGetSize(List* list)
{
    unsigned n = 0;
    while (list != NULL) {
        n++;
        list = list->_next;
    }
    return n;
}

void
ListInsert(List** list_a, List* newList, List* before)
{
	List* l;

	for (l = *list_a; l != NULL && l->_next != before; l = l->_next)
		;

	if (l != NULL) {
		l->_next = newList;
        newList->_next = before;
    } else if (*list_a == before) {
		*list_a = newList;
        newList->_next = before;
    } /* else we had a before but never found it */
}

void
ListAppend(List** list_a, List* newList)
{
    ListInsert(list_a, newList, NULL);
}

void
ListPush(List** list_a, List* newList)
{
	newList->_next = *list_a;
	*list_a = newList;
}

List*
ListPop(List** list_a)
{
	List* r;
	if (*list_a != NULL) {
		r = *list_a;
		*list_a = r->_next;
	} else {
		r = NULL;
	}
	return r;
}

List*
ListFind(List* list, void* thing)
{
	for (; list != NULL; list = list->_next) {
		if (list->thing == thing)
			return list;
	}
	return NULL;
}

static List* freeList;

void
ListFree(List* list)
{
	ListPush(&freeList, list);
}

void
ListRemove(List** list_a, List* list) /* remove a list from chain */
{
    List* l;
    List* m;

    for (l = *list_a, m = NULL; l != NULL; m = l, l = l->_next) {
        if (l == list)
            break;
    }

    if (l != NULL) {
        if (m != NULL)
            m->_next = l->_next;
        else
            *list_a = l->_next;
        ListFree(l);
    }
}

#define LIST_ALLOC_CHUNKCOUNT 64

List*
ListNew(void* thing)
{
	List* l;

	l = ListPop(&freeList);

	if (l == NULL) {
        int i;
		l = MEM_VECTOR(List, LIST_ALLOC_CHUNKCOUNT);
        for (i = 0; i < LIST_ALLOC_CHUNKCOUNT; i++) {
            ListPush(&freeList, l);
            l++;
        }
        l = ListPop(&freeList);
    }

    l->_next = NULL;
    l->thing = thing;

    return l;
}

typedef List SegmentList;
typedef List ObjectList;
typedef List SymbolList;

#define SegmentListNew(x)        ListNew((Segment*)(x))
#define SegmentListGetSegment(x) (Segment*)(x)->thing
#define ObjectListNew(x)         ListNew((Object*)(x))
#define ObjectListGetObject(x)   (Object*)(x)->thing
#define SymbolListNew(x)         ListNew((Symbol*)(x))
#define SymbolListGetSymbol(x)   (Symbol*)(x)->thing

#define SEGMENT_HAS_STDLIB 0x1
#define STDLIB_SIZE (1024*8)

typedef struct Segment
{
    ObjectList* objects;
    SymbolList* jmpSymbols;
    SymbolList* surrogateSymbols;
    long        dataBegin;
    long        dataEnd;
    bfd_size_type textSize;
    bfd_size_type bssSize;
    bfd_size_type dataSize;
    unsigned char* dataData;
    int           segmentNumber;
    unsigned    flags;
} Segment;

Segment*
SegmentNew()
{
    Segment* newGuy = MEM_NEWZAP(Segment);
    newGuy->segmentNumber = -1;
    return newGuy;
}

#define OBJECT_IN_ARCHIVE 0x1
#define OBJECT_REFERENCED 0x2

typedef struct Object
{
    Segment*    segment;
    ObjectList* users;
    ObjectList* dependencies;
    bfd_size_type textSize;
    char*       name;
    SymbolList* symbols;
    bfd*        bfdData;
    unsigned    flags;
} Object;

Object*
ObjectNew(char* name, bfd* bfdData)
{
    Object* newGuy = MEM_NEWZAP(Object);
    newGuy->name = strdup(name);
    newGuy->bfdData = bfdData;
    return newGuy;
}

char*
ObjectGetName(Object* object)
{
    return object->name;
}

#define BFD_IS_COMMON    0x20000
#define BFD_IS_DATA      0x40000
#define BFD_IS_UNDEFINED 0x80000

typedef struct Symbol
{
    struct Symbol* _next;

    char*       name;
    unsigned    bfdFlags;
    bfd_size_type size;
    /* other stuff */

    Object*     object;
    ObjectList* users; /* maybe wont use, see object */
} Symbol;

Symbol*
SymbolNew(char* name, flagword bfdFlags, Object* object)
{
    Symbol* newGuy = MEM_NEWZAP(Symbol);
    newGuy->name = strdup(name);
    newGuy->bfdFlags = bfdFlags;
    newGuy->object = object;
    newGuy->_next = NULL;
    newGuy->users = NULL;
    return newGuy;
}

#define SymbolNewUndefined(n) SymbolNew((n), 0, NULL)

typedef struct SymbolTable
{
    Symbol** heads;
    unsigned size;
    unsigned nentries;
} SymbolTable;

static unsigned long
hash_function(const void *xv)
{ 
    unsigned long h = 0;
    unsigned long g;
    unsigned const char *x = (const char *) xv;

    if (!x)
        return 0;

    while (*x != 0) {
        h = (h << 4) + *x++;
        if ((g = h & 0xf0000000) != 0)
            h = (h ^ (g >> 24)) ^ g;
    }

    return h;
}

SymbolTable*
SymbolTableNew(unsigned p_size)
{
    SymbolTable* table = MEM_NEW(SymbolTable);

    unsigned size;
    for (size = 0x1; size < (16*1024); size <<= 1) {
        if (size >= p_size)
            break;
    }
    table->size = size;
    table->heads = (Symbol**)MEM_CALLOC(size, sizeof(Symbol*));
    table->nentries = 0;

    return table;
}

Symbol*
SymbolTableInsert(SymbolTable* table, Symbol* sym)
{
    unsigned long hash = hash_function(sym->name);
    unsigned long mask = table->size - 1;
    unsigned index = (hash & mask);

    sym->_next = table->heads[index];
    table->heads[index] = sym;
    table->nentries++;

    return sym;
}

Symbol*
SymbolTableFind(SymbolTable* table, char* name)
{
    Symbol* sym;
    Symbol* head;

    unsigned long hash = hash_function(name);
    unsigned long mask = table->size - 1;
    unsigned index = (hash & mask);
    head = table->heads[index];

    for (sym = head; sym != NULL; sym = sym->_next) {
        if (strcmp(name, sym->name) == 0)
            break;
    }

    return sym;
}

typedef int (*eh_dump_mappee_t)(Symbol* sym, void* arg);

static int
SymbolTableMap(SymbolTable* table, eh_dump_mappee_t func, void* arg)
{
    Symbol* sym;
    Symbol* head;

    unsigned n;
    for (n = 0; n < table->size; n++) {
        head = table->heads[n];
        for (sym = head; sym != NULL; sym = sym->_next) {
            if ((func)(sym, arg) == -1)
                return -1;
        }
    }

    return 0;
}

typedef List LibdirList;
#define LibdirListNew(x)         ListNew((Libdir*)(x))
#define LibdirListGetLibdir(x)   (Libdir*)(x)->thing
#define LibraryListNew(x)        ListNew((void*)(x))
#define LibraryListGetLibrary(x) (const char*)(x)->thing

static const char*
library_lookup(char* name, LibdirList* libdirList, const char* basename)
{
    LibdirList* l;
    const char* rv = NULL;
    
    for (l = libdirList; l != NULL; l = l->_next) {
        const char* dir = LibraryListGetLibrary(l);
        int len = strlen(dir);
        strcpy(name, dir);
        if (name[len-1] != '/')
            strcat(name, "/");
        strcat(name, "lib");
        strcat(name, basename);
        strcat(name, ".a");
        if (access(name, R_OK) == 0) {
            rv = (const char*)name;
            break;
        }
    }

    return rv;
}

static void
library_add_paths(LibdirList** libdirList, const char* path)
{
    const char* p = path;
    for (;;) {
        const char* q = strchr(p, ':');
        char* s;
        int len;
        if (q != NULL)
            len = q - p;
        else
            len = strlen(p);
        s = (char*)malloc(len + 1);
        memmove(s, p, len);
        s[len] = '\0';
        ListAppend(libdirList, LibraryListNew(s));
        if (q != NULL)
            p = q + 1;
        else
            break;
    }
}

typedef struct LibName
{
    char* basename;
    char* libname;
} LibName;

typedef List LibNameList;

#define LibNameListNew(x)        ListNew((LibName*)(x))
#define LibNameListGetLibName(x) (LibName*)(x)->thing

static LibNameList* theLibNameList;
static char* libDirVar;
static char* palmGcc;
static unsigned globalsInA4;
static unsigned relocateIs05;

static char*
get_library_filename(char* basename)
{
    LibNameList* list;
    LibName*     lib;

    for (list = theLibNameList; list != NULL; list = list->_next) {
        lib = LibNameListGetLibName(list);
        if (strcmp(lib->basename, basename) == 0) {
            return lib->libname;
        }
    }

    if (libDirVar == NULL) {
        char* env = getenv("MULTILINK_LIBDIR");
        if (env == NULL)
            env = MULTILINK_LIBDIR;
        libDirVar = strdup(env);
    }

    lib = MEM_NEWZAP(LibName);
    lib->basename = strdup(basename);
    lib->libname = (char*)MEM_MALLOC(strlen(libDirVar) +
                                     strlen(basename) +
                                     2);

    strcpy(lib->libname, libDirVar);
    strcat(lib->libname, "/");
    strcat(lib->libname, basename);

    list = LibNameListNew(lib);
    ListPush(&theLibNameList, list);

    return lib->libname;
}

/* The stuff that does something */
typedef struct {
    SymbolTable* symbols;
    SegmentList* segment;
    ObjectList*  objects;
} World;
  
static asymbol**
bfd_get_symbols(bfd* theBfd, unsigned* nSymbols_a)
{
    long      storage_needed;
    asymbol** symbol_table;
    long      number_of_symbols;

    storage_needed = bfd_get_symtab_upper_bound(theBfd);

    if (storage_needed <= 0)
        return NULL;

    symbol_table = (asymbol **)MEM_MALLOC(storage_needed);

    number_of_symbols =	bfd_canonicalize_symtab(theBfd, symbol_table);

    if (number_of_symbols < 0) {
        MEM_FREE(symbol_table);
        return NULL;
    }

    *nSymbols_a = number_of_symbols;

    return symbol_table;
}

typedef struct EmitSegment
{
    const char* filename;
    bfd*        bfd;     /* the segment object */
    asymbol**   symbols; /* all the symbols in the segment */
    unsigned    nSymbols;
    asection*   data_section;
    asection*   reloc_section;
    asection*   bss_section;
    asection*   text_section;
} EmitSegment;

typedef struct Relocation
{
    unsigned offset;
    unsigned long adjustment;
} Relocation;

static Relocation*
RelocationNew(unsigned offset, unsigned long adjustment)
{
    Relocation* relocation = MEM_NEW(Relocation);

    relocation->adjustment = adjustment;
    relocation->offset = offset;

    return relocation;
}

#define RelocationListNew(x)           ListNew((Relocation*)(x))
#define RelocationListGetRelocation(x) (Relocation*)(x)->thing

typedef struct EmitCtx
{
    const char*   basename;
    EmitSegment*  currentSegment;
    List*         relocations;
    /* options */
    unsigned long cid;
    int           verbosity;
    int           leavetmp;
    int           minusg;
    TargetType    targetType;
} EmitCtx;

static EmitSegment*
EmitSegmentInit(EmitSegment* segment, const char* filename)
{
    bfd*      bfd;
    asymbol** symbols;
    unsigned  nsymbols;

    bfd = bfd_openr(filename, NULL);
    if (bfd_check_format(bfd, bfd_object) == 0) {
        fprintf(stderr, "%s is not an object file\n", filename);
        return NULL;
    }
    symbols = bfd_get_symbols(bfd, &nsymbols);

    segment->filename = filename;
    segment->bfd = bfd;
    segment->symbols = symbols;
    segment->nSymbols = nsymbols;
    segment->data_section = bfd_get_section_by_name(bfd, ".data");
    segment->reloc_section = bfd_get_section_by_name(bfd, ".reloc");
    segment->bss_section = bfd_get_section_by_name(bfd, ".bss");
    segment->text_section = bfd_get_section_by_name(bfd, ".text");

    return segment;
}

static void
EmitSegmentFinalize(EmitSegment* segment)
{
    bfd_close(segment->bfd);
}

static char*
make_resource_file_name(char* buf, const char* base,
                        const char* res_type, unsigned short res_id)
{
    sprintf(buf, "%s%04x.", res_type, res_id);
    if (base != NULL) {
        strcat(buf, base);
        strcat(buf, ".");
    }
    strcat(buf, "grc");
    return buf;
}

static void
EmitResource(EmitCtx* eCtx,
             const char* res_type, unsigned short res_id,
             const void* buf, unsigned bufsize)
{
    char filename[MAXPATHLEN];
    FILE* fp;

    make_resource_file_name(filename, eCtx->basename, res_type, res_id);

    fp = fopen(filename, "wb");

    fwrite(buf, 1, (size_t)bufsize, fp);

    fclose(fp);
}

static EmitCtx*
EmitCtxInit(EmitCtx*      eCtx,
            const char*   basename,
            unsigned long cid,
            int           verbosity,
            int           leavetmp,
            int           minusg,
            TargetType    targetType)
{
    eCtx->currentSegment = NULL;
    eCtx->basename = basename;
    eCtx->relocations = NULL;

    /* options */
    eCtx->cid = cid;
    eCtx->verbosity = verbosity;
    eCtx->leavetmp = leavetmp;
    eCtx->minusg = minusg;
    eCtx->targetType = targetType;

    return eCtx;
}
            
static void
EmitCtxFinalize(EmitCtx* eCtx)
{
}

static int
bput_u32(bfd_byte** p_a, unsigned long value)
{
    unsigned long buf = htonl(value);
    bfd_byte* p = *p_a;
    bfd_byte* q = (bfd_byte*)&buf;

    *p++ = *q++;
    *p++ = *q++;
    *p++ = *q++;
    *p++ = *q++;

    *p_a = p;

    return 4;
}

static int
bput_u16(bfd_byte** p_a, unsigned short value)
{
    unsigned short buf = htons(value);
    bfd_byte* p = *p_a;
    bfd_byte* q = (bfd_byte*)&buf;

    *p++ = *q++;
    *p++ = *q++;

    *p_a = p;

    return 2;
}

#if 0
static int
bput_u8(bfd_byte** p_a, unsigned value)
{
    unsigned char buf = value;
    bfd_byte* p = *p_a;

    *p++ = buf;

    *p_a = p;

    return 1;
}
#endif

static bfd_byte*
bfd_get_relocs_20(EmitCtx* eCtx, unsigned long* result_size_rv)
{
    EmitSegment* segment = eCtx->currentSegment;
    asection* data_section = segment->data_section;
    asection* reloc_section = segment->reloc_section;
    asection* bss_section = segment->bss_section;
    asection* text_section = segment->text_section;
    bfd*      input_bfd = segment->bfd;

    /* we only support relocations in the data section */
    bfd_size_type reloc_size;
    bfd_byte*     reloc_vector;
    bfd_byte*     rel;
    unsigned long num_pilot_relocs = 0;
    bfd_byte*     result;
    bfd_byte*     p;

    *result_size_rv = 0;

    if (!reloc_section)
        return 0;

    reloc_size = bfd_section_size(input_bfd, reloc_section);

    if (reloc_size < 0)
        return NULL;

    reloc_vector = (bfd_byte*)MEM_MALLOC((size_t)reloc_size);

    if (!bfd_get_section_contents(input_bfd,
                                  reloc_section, reloc_vector,
                                  0, reloc_size)) {
        return NULL;
    }

    result = (bfd_byte*)MEM_MALLOC(2 /* short for nSymbols */ +
                                   (((reloc_size/12)+1)*sizeof(pilot_reloc)));
    p = result + 2;

    for (rel = reloc_vector; rel < reloc_vector + reloc_size; rel += 12) {
        unsigned int  type;
        unsigned long reloffset;
        int           relsecndx;
        int           symsecndx;
        asection*     sec;
        asection*     relsec;
        asection*     symsec;
        pilot_reloc   pr;
        Relocation*   relocation;
        List*         list;
        unsigned long adjustment;

        type      = bfd_get_16(input_bfd, rel);
        relsecndx = bfd_get_16(input_bfd, rel+2);
        reloffset = bfd_get_32(input_bfd, rel+4);
        symsecndx = bfd_get_16(input_bfd, rel+8);

        if (type != 1) {
            fprintf(stderr, "unknown reloc type 0x%x\n", type);
            continue;
        }

        relsec = symsec = NULL;
        for (sec = input_bfd->sections; sec; sec = sec->next) {
            if (sec->index == relsecndx)
                relsec = sec;
            if (sec->index == symsecndx)
                symsec = sec;
        }

        if (!relsec) {
            fprintf(stderr, "reloc in non-data section\n");
            continue;
        }

        if (!symsec) {
            fprintf(stderr, "reloc relative to strange section\n");
            continue;
        }

        pr.type = RELOC_ABS_32;
        if (symsec == text_section) {
            pr.section = TEXT_SECTION;
            adjustment = text_section->vma;
        } else if (symsec == data_section) {
            pr.section = DATA_SECTION;
            adjustment = data_section->vma;
        } else if (symsec == bss_section) {
            pr.section = BSS_SECTION;
            adjustment = bss_section->vma;
        } else {
            fprintf(stderr, "reloc in bad section\n");
            continue;
        }

        if (adjustment != 0) {
            relocation = RelocationNew(reloffset, adjustment);
            list = RelocationListNew(relocation);
            ListPush(&eCtx->relocations, list);
        }

#if 0
        printf("offset = %ld\n", reloffset);
#endif

        pr.offset = htons((unsigned short)reloffset);
        /* We could less. The loader doesn't use it. */
        pr.value = 0;

        memcpy(p, &pr, 8);
        p += 8;
        num_pilot_relocs++;
    }

    MEM_FREE(reloc_vector);

    *result_size_rv = p - result;
    p = result;
    bput_u16(&p, num_pilot_relocs);

    return result;
}

static bfd_byte*
bfd_get_relocs_05(EmitCtx* eCtx, unsigned long* result_size_rv)
{
    EmitSegment* segment = eCtx->currentSegment;
    asection* data_section = segment->data_section;
    asection* bss_section = segment->bss_section;
    asection* text_section = segment->text_section;
    bfd*      input_bfd = segment->bfd;

    /* we only support relocations in the data section */
    bfd_size_type reloc_size;
    arelent**     reloc_vector;
    unsigned long num_pilot_relocs = 0;
    long          reloc_count;
    long          count;
    bfd_byte*     result;
    bfd_byte*     p;

    reloc_size = bfd_get_reloc_upper_bound(input_bfd, data_section);

    *result_size_rv = 0;

    if (reloc_size < 0)
        return NULL;

    reloc_vector = (arelent **)MEM_MALLOC((size_t)reloc_size);

    reloc_count = bfd_canonicalize_reloc(input_bfd,
                                         data_section,
                                         reloc_vector,
                                         segment->symbols);
    if (reloc_count < 0) {
        return NULL;
    }

    if (reloc_count > 0) {
        result = (bfd_byte*)MEM_MALLOC(2 /* short for nSymbols */ +
                                       (reloc_count+1)*sizeof(pilot_reloc));
        p = result + 2;
    } else {
        result = NULL;
    }

#if 0
    printf("reloc_count = %ld\n", reloc_count);
#endif

    for (count = 0; count < reloc_count; count++) {

        arelent*     r = &(*reloc_vector)[count];
        asymbol*     s = *r->sym_ptr_ptr;
        pilot_reloc  pr;
        Relocation*   relocation;
        List*         list;
        unsigned long reloffset;
        unsigned long adjustment;

        if (strncmp(r->howto->name, "32", 2) != 0) {
            fprintf(stderr, "warning: skipping relocation");
            fprintf(stderr, "DATA %s: ", r->howto->name);
            fprintf(stderr, "addr=0x%lx ", (unsigned long)r->address);
            fprintf(stderr, "sym=%s+%lx", s->name, (unsigned long)s->value);
            fprintf(stderr, "\n");
            continue;
        }

        pr.type = RELOC_ABS_32;
        if (s->section == text_section) {
            pr.section = TEXT_SECTION;
            adjustment = text_section->vma;
        } else if (s->section == data_section) {
            pr.section = DATA_SECTION;
            adjustment = data_section->vma;
        } else if (s->section == bss_section) {
            pr.section = BSS_SECTION;
            adjustment = bss_section->vma;
        } else {
            fprintf(stderr, "reloc in bad section\n");
            continue;
        }

        reloffset = r->address - data_section->vma;
        if (adjustment != 0) {
            relocation = RelocationNew(reloffset, adjustment);
            list = RelocationListNew(relocation);
            ListPush(&eCtx->relocations, list);
        }

#if 0
        printf("r->address = 0x%lx, data_section->vma = 0x%lx\n",
               r->address, data_section->vma);
#endif

        pr.offset = htons(reloffset);
        pr.value = htonl(s->value);

        memcpy(p, &pr, 8);
        p += 8;
        num_pilot_relocs++;
    }

    MEM_FREE(reloc_vector);

    if (result != NULL) {
        *result_size_rv = p - result;
        p = result;
        bput_u16(&p, num_pilot_relocs);
    } else {
        *result_size_rv = 0;
    }

    return result;
}

static int
make_rloc_file(EmitCtx* eCtx, const char* res_type, unsigned long res_id)
{
    unsigned long size_pilot_relocs;
    bfd_byte*     pilot_relocs;

    if (relocateIs05) {
        pilot_relocs = bfd_get_relocs_05(eCtx, &size_pilot_relocs);
    } else {
        pilot_relocs = bfd_get_relocs_20(eCtx, &size_pilot_relocs);
    }

    if (size_pilot_relocs != 0)
        EmitResource(eCtx, res_type, res_id, pilot_relocs, size_pilot_relocs);

    if (pilot_relocs != NULL)
        MEM_FREE(pilot_relocs);

    return 0;
}

static const unsigned char exec_magic[] = { 0, 0, 0, 1 };

static int
make_text_file(EmitCtx* eCtx, const char* res_type, unsigned long res_id)
{
    EmitSegment* segment = eCtx->currentSegment;
    bfd*      text_bfd = segment->bfd;
    asection* text_section;
    bfd_byte* text_vector;
    bfd_size_type text_size;

    text_section = segment->text_section;
    text_size = bfd_section_size(text_bfd, text_section);
    text_vector = (bfd_byte*)MEM_MALLOC((size_t)text_size + 4);

    if (!bfd_get_section_contents(text_bfd, text_section, text_vector + 4,
                                  0, text_size)) {
        fprintf(stderr, "%s: could not read text section\n",
                segment->filename);
        return -1;
    }

    memcpy(text_vector, exec_magic, 4);

    EmitResource(eCtx, res_type, res_id, text_vector, text_size + 4);

    MEM_FREE(text_vector);

    return 0;
}

static int
make_code0(EmitCtx* eCtx)
{
    EmitSegment* segment = eCtx->currentSegment;
    bfd_size_type data_size;
    bfd_size_type bss_size;
    size_t total_data_size;
    char  buf[24];
    bfd_byte* p = buf;

#if 0
    /* does not work on 0.5 objects */
    data_size = bfd_section_size(segment->bfd, segment->data_section);
    bss_size  = bfd_section_size(segment->bfd, segment->bss_section);
#else
    bfd_size_type data_vma = segment->data_section->vma;
    bss_size = segment->bss_section->_raw_size;
    data_size = segment->bss_section->vma - data_vma;
#endif

    /* Round up to the next longword boundary.  */
    total_data_size = (data_size + bss_size + 3) & ~3;

    bput_u32(&p, 0x00000028); /* data size above %a5 (?) */
    bput_u32(&p, total_data_size); /* total data size */
    bput_u32(&p, 8);	      /* size of jump table (?) */
    bput_u32(&p, 0x00000020); /* jump table's offset from %a5 (?) */

    /* The one and only jump table entry: (?) */

    bput_u16(&p, 0x0000);     /* offset (?) */
    bput_u16(&p, 0x3f3c);	  /* move.w #IMM,-(%sp) (?) */
    bput_u16(&p, 0x0001);	  /* IMM = 1 (?) */
    bput_u16(&p, 0xa9f0);	  /* Macintosh SegLoad trap (?!) */

    EmitResource(eCtx, "code", 0, buf, 24);

    return 0;
}

/*
 * The pref resource contains a SysAppPrefsType, as described in
 * System/SystemPrv.h:
 * typedef struct SysAppPrefs {
 *   Word		priority;					// task priority
 *   DWord		stackSize;					// required stack space
 *   DWord		minHeapSpace;				// minimum heap space required
 * } SysAppPrefsType;
 *
 */
static int
make_pref(EmitCtx* eCtx, unsigned long stack) /* 4096 */
{
    char  buf[10];
    bfd_byte* p = buf;

    bput_u16(&p, DEFAULT_AMX_PRIORITY);
    bput_u32(&p, stack);
    bput_u32(&p, DEFAULT_HEAP_SIZE);

    EmitResource(eCtx, "pref", 0, buf, 10);

    return 0;
}

static int
make_code_resource_files(EmitCtx* eCtx,
                         char* foo_filename,
                         unsigned segmentIndex, TargetType targetType)
{
    unsigned short id;
    const char*    type;
    EmitSegment    segmentBuf;
    EmitSegment*   segment;

    segment = EmitSegmentInit(&segmentBuf, foo_filename);
    eCtx->currentSegment = segment;

    /* code (text) text resource */
    type = "code";
    id = (segmentIndex == 0)? 1: segmentIndex + 0x8000;
    if (segmentIndex == 0 && targetType == TARGET_SYSLIB) {
        type = "libr";
        id = 0;
    }
    make_text_file(eCtx, type, id);

    /* rloc (relations) resource */
    id = (segmentIndex == 0)? 0: segmentIndex + 0x8000;
    make_rloc_file(eCtx, "rloc", id);

    if (segmentIndex == 0) {
        /* code0000 */
        make_code0(eCtx);
        
        /* pref0000 */
        if (targetType != TARGET_SYSLIB) {
            make_pref(eCtx, DEFAULT_HEAP_SIZE);
        }
    }

    EmitSegmentFinalize(segment);
        
    return 0;
}

static void
init_symbols(SymbolTable* symbols)
{
    Object* object;
    Symbol* symbol;

    object = ObjectNew("*builtin*", NULL);

    symbol = SymbolNew("edata", 0x0, object);
    SymbolTableInsert(symbols, symbol);
    ListAppend(&object->symbols, SymbolListNew(symbol));

    symbol = SymbolNew("MultilinkSegmentJmpTables", 0x0, object);
    symbol->bfdFlags |= BFD_IS_COMMON;
    symbol->size = 4;
    SymbolTableInsert(symbols, symbol);
    ListAppend(&object->symbols, SymbolListNew(symbol));

}

static char* djwDebug;

static Object*
process_object(World* theWorld, bfd* bf)
{
    char*     filename = bfd_get_filename(bf);
    asection* textSection;
    Object*   object;
    unsigned n;
    unsigned nSymbols;
    asymbol** bfdSymbols;

    object = ObjectNew(filename, bf);

    ListAppend(&theWorld->objects, ObjectListNew(object));

    textSection = bfd_get_section_by_name(bf, ".text");

    object->textSize = bfd_get_section_size_before_reloc(textSection);

    bfdSymbols = bfd_get_symbols(bf, &nSymbols);

    for (n = 0; n < nSymbols; n++) {
        asymbol* sym = bfdSymbols[n];
        asection* sec = bfd_get_section(sym);
        Symbol* symbol;
        unsigned global;

#if 0
        printf("symbol: %s:%s 0x%x %s\n",
               filename, sym->name, sym->flags, sec->name);
#endif

        if (djwDebug != NULL && strcmp(sym->name, djwDebug) == 0) {
            printf("found %s\n", djwDebug);
        }

#define IS_UNKNOWN(s) ((s)->object == NULL)
#define IS_DATA_SECTION(s) (strcmp((s)->name, ".data")==0)
#define IS_COM_SECTION(s) (bfd_is_com_section(s))
#define IS_TEXT(s)    (((s)->bfdFlags & BSF_GLOBAL) != 0 && \
                       ((s)->bfdFlags & BFD_IS_COMMON) == 0 && \
                       ((s)->bfdFlags & BFD_IS_DATA) == 0)

        global = ((sym->flags & BSF_GLOBAL) != 0) || bfd_is_com_section(sec);

        if (global) {/* export */
            symbol = SymbolTableFind(theWorld->symbols, (char*)sym->name);

            if (symbol != NULL) {
                if (symbol->object == NULL) {
                    symbol->object = object;
                    ListAppend(&object->symbols, SymbolListNew(symbol));
                    symbol->bfdFlags = sym->flags;

                    if (IS_COM_SECTION(sec)) {
                        symbol->bfdFlags |= BFD_IS_COMMON;
                        symbol->size = sym->value;
                        ListFree(symbol->users);
                        symbol->users = NULL;
                    } else if (IS_DATA_SECTION(sec)) {
                        symbol->bfdFlags |= BFD_IS_DATA;
                        ListFree(symbol->users);
                        symbol->users = NULL;
                    } else { /* text, append object users from symbol users */
                        ObjectList* userL = symbol->users;
                        for (; userL != NULL; userL = userL->_next) {
                            Object* user = ObjectListGetObject(userL);
                            if (ListFind(object->users, user) == NULL)
                                ListPush(&object->users, ObjectListNew(user));
                        }
                    }
                } else { /* huh */
                    fprintf(stderr,
                            "symbol %s defined in %s and redefined in %s\n",
                            symbol->name,
                            ObjectGetName(symbol->object),
                            ObjectGetName(object));
                    return NULL;
                }
            } else {
                symbol = SymbolNew((char*)sym->name, sym->flags, object);
                if (IS_COM_SECTION(sec)) {
                    symbol->bfdFlags |= BFD_IS_COMMON;
                    symbol->size = sym->value;
                } else if (IS_DATA_SECTION(sec)) {
                    symbol->bfdFlags |= BFD_IS_DATA;
                }

                SymbolTableInsert(theWorld->symbols, symbol);
                ListAppend(&object->symbols, SymbolListNew(symbol));
            }
        } else if (bfd_is_und_section(sec)) { /* unknown */
            symbol = SymbolTableFind(theWorld->symbols, (char*)sym->name);

            if (!symbol) { /* not defined yet */
                symbol = SymbolNewUndefined((char*)sym->name);
                SymbolTableInsert(theWorld->symbols, symbol);
            }

            /* add a dependancy from this object to symbol */
            if (IS_TEXT(symbol) || IS_UNKNOWN(symbol)) {
                if (ListFind(symbol->users, object) == NULL)
                    ListPush(&symbol->users, ObjectListNew(object));
                if (ListFind(object->dependencies, object) == NULL)
                    ListPush(&object->dependencies, SymbolListNew(symbol));
            }

            /* add a dependancy from this object to symbol's object */
            if (symbol->object != NULL && symbol->object != object &&
                IS_TEXT(symbol)) {
                if (ListFind(symbol->object->users, object) == NULL)
                    ListPush(&symbol->object->users, ObjectListNew(object));
            }
			
        } /* else we could care less */
    }

    MEM_FREE(bfdSymbols);

    return object;
}

static int
dump_defined_symbols(Symbol* sym, void* arg)
{
    if (sym->object != NULL) {
        ObjectList* userL;

        printf("symbol %s:%s used by:\n",
               ObjectGetName(sym->object), sym->name);
		
        for (userL = sym->users; userL != NULL; userL = userL->_next) {
            Object* user = ObjectListGetObject(userL);

            printf("  %s\n", ObjectGetName(user));
        }
    }
    return 0;
}

static int
dump_undefined_symbols(Symbol* sym, void* arg)
{
    if (sym->object == NULL) {
        ObjectList* userL;

        printf("undefined symbol:%s used by:\n", sym->name);
		
        for (userL = sym->users; userL != NULL; userL = userL->_next) {
            Object* user = ObjectListGetObject(userL);

            printf("  %s\n", ObjectGetName(user));
        }
    }
    return 0;
}

static int
dump_symbol_info(SymbolTable* symbols)
{
    SymbolTableMap(symbols, dump_defined_symbols, NULL);
    SymbolTableMap(symbols, dump_undefined_symbols, NULL);
    return 0;
}

static int
dump_unused_symbols_mappee(Symbol* sym, void* arg)
{
    if (sym->object != NULL && !sym->users) {
        printf("%s:%s\n", ObjectGetName(sym->object), sym->name);
    }
    return 0;
}

static int
dump_unused_symbols(SymbolTable* symbols)
{
    printf("unused symbols:\n");
    SymbolTableMap(symbols, dump_unused_symbols_mappee, NULL);
    printf("end of unused symbols\n");
    return 0;
}

static int
dump_unused_objects(ObjectList* objects)
{
    printf("unused objects:\n");

    for (; objects != NULL; objects = objects->_next) {
		
        Object* object = ObjectListGetObject(objects);

        if (!object->users)
            printf("%s\n", ObjectGetName(object));
    }

    printf("end of unused objects\n");

    return 0;
}

static Object*
find_entry_object(SymbolTable* symbols, TargetType targetType)
{
    Symbol* pilotMainSymbol = NULL;

    if (targetType == TARGET_APP) {
        pilotMainSymbol = SymbolTableFind(symbols, "PilotMain");
        if (pilotMainSymbol == NULL) {
            fprintf(stderr, "no PilotMain() found\n");
            exit(2);
        }
    } else if (targetType == TARGET_SYSLIB) {
        pilotMainSymbol = SymbolTableFind(symbols, "ShlibMain");
        if (pilotMainSymbol == NULL) {
            fprintf(stderr, "no ShlibMain() found\n");
            exit(2);
        }
    }
    return pilotMainSymbol->object;
}

static void
remove_user_reference(ObjectList* objects, Object* user)
{
    for (; objects != NULL; objects = objects->_next) {
	
        Object* object = ObjectListGetObject(objects);
        List*   list = ListFind(object->users, user);
        if (list != NULL)
            ListRemove(&object->users, list);	    
    }
}

static void
remove_unused_mark(Object* object)
{
    SymbolList* dList;

    if (!object || (object->flags & OBJECT_REFERENCED) != 0)
        return;

    object->flags |= OBJECT_REFERENCED;

    for (dList = object->dependencies; dList != NULL; dList = dList->_next) {
        Symbol* dependency = SymbolListGetSymbol(dList);
        Object* dObject = dependency->object;
        remove_unused_mark(dObject);
    }
}

static void
remove_unused_objects(ObjectList** pobjects,
                      SymbolTable* symbols, TargetType targetType,
                      int verbosity)
{
    Object* entryObject = find_entry_object(symbols, targetType);
    ObjectList* objects;

    remove_unused_mark(entryObject);

    do {
        for (objects = *pobjects; objects != NULL; objects = objects->_next) {
	    
            Object* object = ObjectListGetObject(objects);
	    
            if ((object->flags & OBJECT_REFERENCED) == 0) {
                remove_user_reference(*pobjects, object);
                ListRemove(pobjects, objects);
                if (verbosity > 0) {
                    printf("removing unused object %s\n",
                           ObjectGetName(object));
                }
                break;
            }
        }
    } while (objects != NULL);
}

static int
dump_object_info(ObjectList* objects)
{
    for (; objects != NULL; objects = objects->_next) {
		
        ObjectList* userL;
        Object* object = ObjectListGetObject(objects);

        printf("object %s used by:\n", ObjectGetName(object));
		
        for (userL = object->users; userL != NULL; userL = userL->_next) {
            Object* user = ObjectListGetObject(userL);

            printf("  %s\n", ObjectGetName(user));
        }
		
    }
    return 0;
}

static SegmentList*
segment_list_fixups(SegmentList* pSegmentList, SymbolTable* symbols,
                    TargetType targetType)
{
    Symbol* pilotMainSymbol = NULL;
    Object*  object;
    Segment* pmSegment;
    SegmentList* segmentList;
    int index = 0;

    if (targetType == TARGET_APP) {
        pilotMainSymbol = SymbolTableFind(symbols, "PilotMain");
        if (pilotMainSymbol == NULL) {
            fprintf(stderr, "no PilotMain() found\n");
            exit(2);
        }
    } else if (targetType == TARGET_SYSLIB) {
        pilotMainSymbol = SymbolTableFind(symbols, "ShlibMain");
        if (pilotMainSymbol == NULL) {
            fprintf(stderr, "no ShlibMain() found\n");
            exit(2);
        }
    }

    if (pilotMainSymbol != NULL) {
        object = pilotMainSymbol->object;
        pmSegment = object->segment;
	
        segmentList = ListFind(pSegmentList, pmSegment);
        if (segmentList != pSegmentList) {
            ListRemove(&pSegmentList, segmentList);
            ListPush(&pSegmentList, SegmentListNew(pmSegment));
        }
    }
    
    for (index = 0, segmentList = pSegmentList;
         segmentList != NULL;
         segmentList = segmentList->_next) {
        Segment* segment = SegmentListGetSegment(segmentList);

        segment->segmentNumber = index++;
    }

    return pSegmentList;
}

static SegmentList*
make_segment_list_simple(SegmentList* segments,
                         ObjectList* objects,
                         unsigned segmentMaxSize)
{
    ObjectList*  objectList = objects;

    /* making segments */
    while (objectList != NULL) {
        Segment* segment = SegmentNew();
        segment->textSize = 0;

        /* walking object list */
        for (; objectList != NULL; objectList = objectList->_next) {
            Object* object = ObjectListGetObject(objectList);

            if (object->segment != NULL) /* already assigned */
                continue;

            if (object->textSize > segmentMaxSize) { /* it'll never work */
                fprintf(stderr,
                        "object: %s is larger than maximum segment size\n",
                        ObjectGetName(object));
                exit(3);
            }

            if (segment->textSize + object->textSize > segmentMaxSize) {
                break;
            }

            segment->textSize += object->textSize;
            ListAppend(&segment->objects, ObjectListNew(object));
            object->segment = segment;
        }

        ListPush(&segments, SegmentListNew(segment));
    }

    return segments;
}

/*
 * Format is:
 *   <segment>
 *   object
 *   object
 *   ...
 *   <segment>
 *   object
 *   ...
 *   # comment
 */
static SegmentList*
make_segment_list_from_mapfile(FILE* fp,
                               ObjectList* objects, unsigned segmentMaxSize)
{
    SegmentList* segments = NULL;
    char filename[MAXPATHLEN];
    Segment* segment = NULL;
    unsigned segmentn = 0;
    unsigned yes;
    ObjectList* objectList;
    Object* object;

    while (fgets(filename, MAXPATHLEN, fp) != NULL) {
        char* p;

        p = strchr(filename, '\n');

        if (p == NULL) {
            fprintf(stderr, "bad or way long line in segment map file");
            return NULL;
        }
        *p = '\0';
        
        for (p = filename; isspace(*p); p++)
            ;

        if (*p == '#' || *p == '\0') { /* comment or blank line */
            ;
        } else if (strncmp(filename, "<segment>", 9) == 0) {
            /* start a new segment */
            if (segment != NULL) {
                ListPush(&segments, SegmentListNew(segment));
                segmentn++;
            }
            segment = SegmentNew();
            segment->textSize = 0;
        } else {
            /* else, must be an object line */
            objectList = objects;
            object = NULL;
            for (; objectList != NULL; objectList = objectList->_next) {
                object = ObjectListGetObject(objectList);
                if (strcmp(ObjectGetName(object), p) == 0) {
                    break;
                }
            }

            if (objectList == NULL) {
                fprintf(stderr,
                        "segment map file specifies unknown object '%s'\n",
                        p);
                return NULL;
            }

            if (segment->textSize + object->textSize > segmentMaxSize) {
                fprintf(stderr,
                        "segment #%d exceeds maximim segment size with '%s'\n",
                        segmentn,
                        p);
                return NULL;
            }

            if (object->segment != NULL) {
                fprintf(stderr,
                        "object '%s' assigned to more than one segment\n",
                        p);
                return NULL;
            }

            if (!segment) { /* must be first */
                segment = SegmentNew();
                segment->textSize = 0;
            }
 
            segment->textSize += object->textSize;
            ListPush(&segment->objects, ObjectListNew(object));
            object->segment = segment;
        }
    }

    /* add last segment */
    if (segment != NULL) {
        ListPush(&segments, SegmentListNew(segment));
        segmentn++;
    }

    /*
     * ok, if there are any objects left, go add them in there own little
     * segment.
     */
    yes = 0;
    objectList = objects;
    for (; objectList != NULL; objectList = objectList->_next) {
        object = ObjectListGetObject(objectList);
        if (object->segment == NULL) {
            yes++;
            break;
        }
    }

    if (yes) {
        segments = make_segment_list_simple(segments,
                                            objects,
                                            segmentMaxSize);
    }
    return segments;
}

static SymbolList*
make_segment_jmptable_list(Segment* segment)
{
    /*
     * jmptable list is all the symbols exported from this segment
     * which have a user in another segment. Segments must have been
     * assigned to all objects before calling this function.
     */
    ObjectList* objects = segment->objects;
    SymbolList* jmpTableSymbols = NULL;

    for (; objects != NULL; objects = objects->_next) {
        Object* object = ObjectListGetObject(objects);
        SymbolList* symbols = object->symbols;
        for (; symbols != NULL; symbols = symbols->_next) {
            Symbol* symbol = SymbolListGetSymbol(symbols);
            if (IS_TEXT(symbol)) {
                ObjectList* userL = symbol->users;
                unsigned used = 0;
                for (; userL != NULL; userL = userL->_next) {
                    Object* user = ObjectListGetObject(userL);
                    /* segments MUST have been assigned */
                    if (user->segment != segment) {
                        used++;
                        break;
                    }
                }
                if (used != 0)
                    ListPush(&jmpTableSymbols, SymbolListNew(symbol));
#if 0
                else
                    printf("symbol %s:%s not externally used\n",
                           ObjectGetName(symbol->object), symbol->name);
#endif
                    
            }
        }
    }

    return jmpTableSymbols;
}

typedef struct {
    Segment*    segment;
    SymbolList* symbols;
} surrogate_info;

static int
surrogate_mappee(Symbol* symbol, void* arg)
{
    surrogate_info* info = (surrogate_info*)arg;
    Segment* segmentOfInterest = info->segment;
    
	if (IS_TEXT(symbol) && !IS_UNKNOWN(symbol)) {

        Object* object = symbol->object;

        if (object->segment != segmentOfInterest) {
            ObjectList* userL = symbol->users;
            unsigned used = 0;

            for (; userL != NULL; userL = userL->_next) {
                Object* user = ObjectListGetObject(userL);

                if (user->segment == segmentOfInterest) {
                    used++;
                    break;
                }
            }

            if (used != 0) {
                ListPush(&info->symbols, SymbolListNew(symbol));
            }
        }
	}
	return 0;
}

static SymbolList*
make_segment_surrogate_list(Segment* segment, SymbolTable* symbols)
{
    /*
     * surrogate list is all the symbols used by this segment
     * which are in another segment. Segments must have been
     * assigned to all objects before calling this function.
     */
    surrogate_info info;

    info.segment = segment;
    info.symbols = NULL;

	SymbolTableMap(symbols, surrogate_mappee, &info);

    return info.symbols;
}

static int
segments_make_jmptable_and_surrogates(SegmentList* segments,
                                      SymbolTable* symbols)
{
    for (; segments != NULL; segments = segments->_next) {
        Segment* segment = SegmentListGetSegment(segments);

        if (segment->jmpSymbols != NULL)
            ListFree(segment->jmpSymbols);
        if (segment->surrogateSymbols != NULL)
            ListFree(segment->surrogateSymbols);

        segment->jmpSymbols = make_segment_jmptable_list(segment);
        segment->surrogateSymbols = make_segment_surrogate_list(segment,
                                                                symbols);
    }
    return 0;
}

#if 0
static int
dump_symbol_list(SymbolList* symbols)
{
    for (; symbols != NULL; symbols = symbols->_next) {
        Symbol* symbol = SymbolListGetSymbol(symbols);
        printf("%s:%s\n",
               ObjectGetName(symbol->object), symbol->name);
    }
    return 0;
}
#endif

static int
dump_segments(SegmentList* segments)
{
    int i = 0;
	for (; segments != NULL; segments = segments->_next, i++) {
		
		Segment*    segment = SegmentListGetSegment(segments);
		ObjectList* objects = segment->objects;
        unsigned    segmentSize = 0;
        SymbolList* symbols;
        
        for (; objects != NULL; objects = objects->_next) {
            Object* object = ObjectListGetObject(objects);
            segmentSize += object->textSize;
        }

        printf("segment #%d\n  size = %d\n  objects:\n", i, segmentSize);

        objects = segment->objects;
        for (; objects != NULL; objects = objects->_next) {
            Object* object = ObjectListGetObject(objects);
			printf("    %s\n", ObjectGetName(object));
        }

        printf("  exports:\n");
        symbols = segment->jmpSymbols;
        for (; symbols != NULL; symbols = symbols->_next) {
            Symbol* symbol = SymbolListGetSymbol(symbols);
            printf("    %s\n", symbol->name);
        }

        printf("  outside called:\n");
        symbols = segment->surrogateSymbols;
        for (; symbols != NULL; symbols = symbols->_next) {
            Symbol* symbol = SymbolListGetSymbol(symbols);
            printf("    %s:%s\n",
                   ObjectGetName(symbol->object), symbol->name);
        }
    }

    return 0;
}

/*
 * Get the index in this symbol's segment's jmptable of this symbol.
 */
int
symbol_get_jmptable_index(Symbol* thisSymbol)
{
    Object* object = thisSymbol->object;

    if (object != NULL) {
        Segment* segment = object->segment;

        if (segment != NULL) {
            SymbolList* symbolList = segment->jmpSymbols;
            int index = 0;

            for (; symbolList != NULL; symbolList = symbolList->_next) {
                Symbol* symbol = SymbolListGetSymbol(symbolList);
                if (symbol == thisSymbol)
                    return index;
                
                index++;
            }
        }
    }

    return -1;
}

int
segment_write_jmptable(FILE* fp, Segment* segment, unsigned segmentIndex)
{
    SymbolList* symbolList = segment->jmpSymbols;
    int count = 0;
    
#if 0
    /*
     * for now we aleays have to do it, or we get link errors.
     * we need to move to a model that has runtime binding instead
     * of link time binding.
     */
    if (symbolList == NULL)
        return 0; /* nothing in the jmptable */
#endif

    if (segmentIndex == 0) {
        fprintf(fp,
                ".text\n"
                ".even\n"
                ".globl multilink_fake_start\n"
                "multilink_fake_start:\n"
                "    bra.w start\n" /* unconditional branch to start */
                ".globl MultilinkSegmentJmpTable\n"
                "MultilinkSegmentJmpTable:\n");
    } else {
        fprintf(fp,
                ".text\n"
                ".even\n"
                ".globl start\n"
                "start:\n"
                ".globl MultilinkSegmentJmpTable\n"
                "MultilinkSegmentJmpTable:\n");
    }
    
    for (; symbolList != NULL; symbolList = symbolList->_next) {
        Symbol* symbol = SymbolListGetSymbol(symbolList);
        fprintf(fp, "    dc.w %s-MultilinkSegmentJmpTable\n", symbol->name);
        count++;
    }

    fprintf(fp,
            "MultilinkSegmentJmpTable_end:\n");

    return count;
}

static int
segmentlist_get_segment_index(SegmentList* segmentList, Segment* matchSegment)
{
    int index = 0;
    for (; segmentList != NULL; segmentList = segmentList->_next) {
        Segment* segment = SegmentListGetSegment(segmentList);
        if (segment == matchSegment)
            return index;
        index++;
    }
    return -1;
}

static unsigned
segment_list_get_nsegments(SegmentList* segmentList)
{
    unsigned index = 0;
    for (; segmentList != NULL; segmentList = segmentList->_next)
        index++;

    return index;
}

static int
write_dispatch_code_a4(FILE* fp, unsigned long cid)
{
    fprintf(fp,
            /* When we don't have globals, get jmptables from ftr land */
            "dispatch_noglobals:\n"
            "    move.l %%d0,-(%%sp)\n"
            "    move.l %%d1,-(%%sp)\n"
            "    move.l #0x%lx,-(%%sp)\n" /* push creator id */
            "    bsr.w MultilinkGetCodeSegments\n"
            "    lea 4(%%sp),%%sp\n"
            "    move.l (%%sp)+,%%d1\n"
            "    move.l (%%sp)+,%%d0\n"
            "    bra.b dispatch_really\n"

            /*
             * If the code from here to the jmp at the end changes,
             * then the value branch_count is set to must change to
             * match.
             */
            "dispatch:\n"
            /* do we have globals */
            "    move.l %%a4,%%d2\n" /* tst.l %a4 fatal resets */
            "    beq.b dispatch_noglobals\n"
            /* yes, get the jmp table pointer */
            "    move.l MultilinkSegmentJmpTables(%%a4),%%a0\n"

            "dispatch_really:\n"
            /* get the segment table */
            "    move.l (%%a0,%%d1.w),%%a0\n"
            /* get the offset from the table */
            "    move.w (%%a0,%%d0.w),%%d2\n"
            "    jmp (%%a0,%%d2.w)\n"
            "", cid);

    return 20; /*FIXME*/
}

static int
write_dispatch_code_a5(FILE* fp)
{
    fprintf(fp,
            /*
             * If the code from here to the jmp at the end changes,
             * then the value branch_count is set to must change to
             * match.
             */
            "dispatch:\n"
            /* get the jmp table pointer */
            "    move.l MultilinkSegmentJmpTables@END(%%a5),%%a0\n"
            /* get the segment table */
            "    move.l (%%a0,%%d1.w),%%a0\n"
            /* get the offset from the table */
            "    move.w (%%a0,%%d0.w),%%d2\n"
            /* jump to real function */
            "    jmp (%%a0,%%d2.w)\n"
            "");
    return 16; /*FIXME*/
}

static int
segment_write_surrogate_calls(FILE* fp,
                              Segment* segment,
                              SegmentList* segmentList,
                              unsigned long cid)
{
    SymbolList* symbolList = segment->surrogateSymbols;
    int count = 0;
    int branch_count;
    
    if (symbolList == NULL)
        return 0; /* nothing in the surrogates */
    
    fprintf(fp,
            "    .text\n"
            "    .even\n"
            "");

    if (globalsInA4) {
        branch_count = write_dispatch_code_a4(fp, cid);
    } else {
        branch_count = write_dispatch_code_a5(fp);
    }            

    /*
     * This could be more optimal by having each entry set the jmptable
     * index, then branch to a section that sets the segment #, which
     * then branches to dispatch.
     */
    for (; symbolList != NULL; symbolList = symbolList->_next) {
        Symbol* symbol = SymbolListGetSymbol(symbolList);
        Object* object = symbol->object;
        Segment* symbolSegment = object->segment;
        int index = symbol_get_jmptable_index(symbol);
        int segmentIndex = segmentlist_get_segment_index(segmentList,
                                                         symbolSegment);
        unsigned adjustedIndex;
        unsigned adjustedSegmentIndex;

        if (index == -1) { /* not defined in segments jmp table */
            fprintf(stderr, "%s is not defined in it's segment jmp table\n",
                    symbol->name);
            return -1;
        }

        if (segmentIndex == -1) { /* should never happen */
            fprintf(stderr, "segment for %s is not defined in segment list\n",
                    symbol->name);
            return -1;
        }

        fprintf(fp,
                ".globl %s\n"
                "%s:\n",
                symbol->name,
                symbol->name);

        /* index * sizeof(unsigned) */
        adjustedIndex = index<<1;
        /* segmentIndex * sizeof(unsigned*) */
        adjustedSegmentIndex = segmentIndex<<2;

        /* Peephole optimize the linker stub entries! */
        if (adjustedIndex < 0x80) {
            fprintf(fp,
                    "    moveq #%d,%%d0\n", /* jmp table offset */
                    adjustedIndex);
            branch_count += 2;
        } else {
            fprintf(fp,
                    "    move.w #%d,%%d0\n", /* jmp table offset */
                    adjustedIndex);
            branch_count += 4;
        }

        if (adjustedSegmentIndex < 0x80) {
            fprintf(fp,
                    "    moveq #%d,%%d1\n", /* jmp table offset */
                    adjustedSegmentIndex);
            branch_count += 2;
        } else {
            fprintf(fp,
                    "    move.w #%d,%%d1\n", /* jmp table offset */
                    adjustedSegmentIndex);
            branch_count += 4;
        }

        if (branch_count+2 <= 0x80) { /* +2 (include the bra instruction) */
            fprintf(fp,
                    "    bra.b dispatch\n");
            branch_count += 2;
        } else {
            fprintf(fp,
                    "    bra dispatch\n");
            branch_count += 4;
        }

        count++;
    }
    
    return count;
}

#if 1
static int
write_comm_mappee(Symbol* symbol, void* arg)
{
    FILE* fp = (FILE*)arg;
    
	if ((symbol->bfdFlags & BFD_IS_COMMON) != 0) {
        if (strcmp(symbol->name, "MultilinkSegmentJmpTables") != 0) {
            fprintf(fp, "    .comm %s,%ld\n", symbol->name, symbol->size);
        }
    }

	return 0;
}

static int
write_comm_section(FILE* fp, SymbolTable* symbols)
{
	SymbolTableMap(symbols, write_comm_mappee, (void*)fp);
    return 0;
}

#else

typedef struct {
    unsigned nsymbols; /* number of symbols */
    Symbol** symbols;  /* vector of symbols */
} write_comm_info;

static int
write_comm_count_mappee(Symbol* symbol, void* arg)
{
	if ((symbol->bfdFlags & BFD_IS_COMMON) != 0) {
        write_comm_info* info = (write_comm_info*)arg;
        info->nsymbols++;
    }

	return 0;
}

static int
write_comm_add_mappee(Symbol* symbol, void* arg)
{
	if ((symbol->bfdFlags & BFD_IS_COMMON) != 0) {
        write_comm_info* info = (write_comm_info*)arg;
        info->symbols[info->nsymbols++] = symbol;
    }

	return 0;
}

static int
write_comm_sortee(const void* vap, const void* vbp)
{
    const Symbol* a = *(const Symbol**)vap;
    const Symbol* b = *(const Symbol**)vbp;
    
    if (b->size < a->size)
        return -1;
    else if (b->size > a->size)
        return 1;

    return strcmp(b->name, a->name);
}

static int
write_comm_section(FILE* fp, SymbolTable* symbols)
{
    write_comm_info info;
    unsigned n;
#if 0
    fprintf(fp,
            "    .comm bss_start,0\n"
            );
#endif
    info.nsymbols = 0;
	SymbolTableMap(symbols, write_comm_count_mappee, &info);

    if (info.nsymbols == 0)
        return 0;

    info.symbols = MEM_VECTOR(Symbol*, info.nsymbols);
    info.nsymbols = 0;
	SymbolTableMap(symbols, write_comm_add_mappee, &info);

    /* now sort them */
    qsort(info.symbols, info.nsymbols, sizeof(Symbol*), write_comm_sortee);

    /* now write them out */
    for (n = 0; n < info.nsymbols; n++) {
        Symbol* symbol = info.symbols[n];
#if 1
        fprintf(fp, "    .comm %s,%ld\n", symbol->name, symbol->size);
#else
        bfd_size_type i; 
        for (i = 0; i < symbol->size; i++)
            fprintf(fp, "    .comm %s_%04ld,1\n", symbol->name, i);
#endif
    }

    MEM_FREE(info.symbols);
    
    return 0;
}
#endif

static int
data_sortee(const void* vap, const void* vbp)
{
    const asymbol* a = *(const asymbol**)vap;
    const asymbol* b = *(const asymbol**)vbp;
    
    if (a->value < b->value)
        return -1;
    else if (a->value > b->value)
        return 1;

    return strcmp(a->name, b->name);
}

#if 0
static arelent**
bfd_get_section_relocs(bfd* theBfd,
                       asection* sec, asymbol** syms, unsigned* nRelocs)
{
    long relsize;
    arelent** relocs;

    if ((sec->flags & SEC_RELOC) == 0) {
        *nRelocs = 0;
        return NULL;
    }


    relsize = bfd_get_reloc_upper_bound(theBfd, sec);
    if (relsize < 0) {
        fprintf(stderr, "bfd_get_reloc_upper_bound() returned -1\n");
        exit(3);
    }

    relocs = (arelent **) malloc (relsize);
    *nRelocs = bfd_canonicalize_reloc(theBfd, sec, relocs, syms);
    if (*nRelocs < 0) {
        fprintf(stderr, "bfd_canonicalize_reloc() returned -1\n");
        exit(3);
    }

    return relocs;
}
#endif

static int
object_is_undefined(Object* object)
{
    int rv = (strcmp(object->name, "*undefined*") == 0);
    return rv;
}

static int
object_write_data(Object* object, FILE* fp,
                  bfd_size_type* total_data, bfd_size_type* total_bss,
                  bfd_size_type* comp_data, bfd_size_type* comp_bss)
{
    asymbol** bfdSymbols;
    asymbol** section_symbols;
    unsigned n;
    unsigned m;
    unsigned nSymbols;
    unsigned nDataSymbols;
    asection* bss_section;
    bfd_size_type bss_size;
    asection* data_section;
    bfd_size_type data_size;
    unsigned char* data_data;
    char filename[MAXPATHLEN];
    int filename_i = 0;
    bfd_size_type data_read = 0;
    bfd_size_type bss_read = 0;
    /*    int even_done = 0; */

#if 0
    if (object->bfdData == NULL)
        return 0; /* the undefined object */
#endif

    fprintf(fp, "| object %s\n", object->name);

    strcpy(filename, object->name);
    for (n = 0; object->name[n] != '\0'; n++) {
        int c = object->name[n];
        if (isalnum(c))
            filename[n] = c;
        else
            filename[n] = '_';
    }

    bss_section = bfd_get_section_by_name(object->bfdData, ".bss");
    bss_size = bss_section->_raw_size;
#if 0
    fprintf(stderr,
            "size = %ld, size%%4 = %ld\n", bss_size, bss_size%4);
#endif
#if 1
    if (bfd_get_section_alignment(object->bfdData, bss_section) != 2) {
        fprintf(stderr,
                "section alignment: %d\n",
                bfd_get_section_alignment(object->bfdData, bss_section));
    }
#endif
        
    data_section = bfd_get_section_by_name(object->bfdData, ".data");
#if 0
    data_size = bss_section->vma - data_section->vma;
#else
    data_size = bfd_section_size(object->bfdData, data_section);
#endif

#if 0
    if ((data_section->flags & SEC_RELOC) != 0) {
        fprintf(stderr,
                "%s: data section has relocation stuff\n", object->name);
    }
#endif

    /* Allows for up to 32 byte align */
    data_data = MEM_MALLOC(data_section->_raw_size + 32);

    if (bfd_get_section_contents(object->bfdData,
                                 data_section,
                                 data_data,
                                 0,
                                 data_section->_raw_size) == false) {
        fprintf(stderr, "error reading data section for %s\n", object->name);
        return -1;
    }

    bfdSymbols = bfd_get_symbols(object->bfdData, &nSymbols);

#if 0
    /* get relocs. */
    relocs = bfd_get_section_relocs(object->bfdData,
                                    data_section,
                                    bfdSymbols,
                                    &nRelocs);

    /* take a look at the relocs */
    for (n = 0; n < nRelocs; n++) {
        arelent *rentry = &(*relocs)[n];
        asymbol* sym = *rentry->sym_ptr_ptr;

        printf("DATA %s: ", rentry->howto->name);
        printf("addr=0x%lx ", rentry->address);
        printf("sym=%s+0x%lx", sym->name, sym->value);
        printf("\n");
    }
#endif

    /* data */
    for (nDataSymbols = 0, n = 0; n < nSymbols; n++) {
        asymbol* sym = bfdSymbols[n];
        asection* sec = bfd_get_section(sym);

        if (sec == data_section)
            nDataSymbols++;
    }

    section_symbols = (asymbol**)MEM_MALLOC(sizeof(asymbol*) * nDataSymbols);

    for (m = 0, n = 0; n < nSymbols; n++) {
        asymbol* sym = bfdSymbols[n];
        asection* sec = bfd_get_section(sym);

        if (sec == data_section)
            section_symbols[m++] = sym;
    }

    qsort(section_symbols, nDataSymbols, sizeof(asymbol*), data_sortee);

    for (n = 0; n < nDataSymbols; n++) {
        asymbol*  sym = section_symbols[n];
        asection* sec = bfd_get_section(sym);
        symvalue  size;
        const char* name = sym->name;

        if (n + 1 < nDataSymbols) /* not last one */
            size = section_symbols[n+1]->value  - sym->value;
        else
            size = bfd_section_size(object->bfdData, sec) - sym->value;

        if (size != 0) {
            int i;
            unsigned char* this_data;

            fprintf(fp, ".data\n");

            if ((sym->flags & BSF_GLOBAL) != 0) {
                fprintf(fp, ".global %s\n", name);

                fprintf(fp, "%s:\n", name);
            } else {
                if (strcmp(name, ".data") == 0)
                    fprintf(fp, ".data_%s%d:\n",
                            filename,
                            filename_i++); /* unique */
                else
                    fprintf(fp, "%s_%s:\n", name, filename);
            }
            this_data = &data_data[sym->value];

            for (i = 0; i < size; i++)
#if 0
                fprintf(fp, "    .byte 0x%02x\n", (unsigned)this_data[i]);
#else
            /*
             * Only output a zero for surrogate data, as all data gets resolved
             * in ... Relocatables get some funny stuff done to the data by
             * the final link, and it's too hard to match that here. By just
             * doing zero, we let each segment 'own' the definition of data
             * coming from that segment.
             */
            fprintf(fp, "    .byte 0x0\n");
#endif

            data_read += size;
        }
    }

    MEM_FREE(section_symbols);
#if 0
    if (relocs != NULL)
        MEM_FREE(relocs);
#endif

#if 1
    if ((data_size & 0x3) != 0) {
        fprintf(fp, ".data\n");
                
        fprintf(fp, ".data_end_of_object_padding_%s%d:\n",
                filename,
                filename_i++); /* unique */

        while ((data_size & 0x3) != 0) {
            fprintf(fp, "    .byte 0x0\n");
            data_size++;
        }
    }
#endif

#if 1
    bss_read = 0;
    /* local bss */
    for (nDataSymbols = 0, n = 0; n < nSymbols; n++) {
        asymbol* sym = bfdSymbols[n];
        asection* sec = bfd_get_section(sym);

        if (djwDebug != NULL && strcmp(sym->name, djwDebug) == 0) {
            printf("found %s\n", djwDebug);
        }
        if (sec == bss_section)
            nDataSymbols++;
    }

    section_symbols = (asymbol**)MEM_MALLOC(sizeof(asymbol*) * nDataSymbols);

    for (m = 0, n = 0; n < nSymbols; n++) {
        asymbol* sym = bfdSymbols[n];
        asection* sec = bfd_get_section(sym);

        if (sec == bss_section)
            section_symbols[m++] = sym;
    }

    qsort(section_symbols, nDataSymbols, sizeof(asymbol*), data_sortee);

    for (n = 0; n < nDataSymbols; n++) {
        asymbol*  sym = section_symbols[n];
        asection* sec = bfd_get_section(sym);
        symvalue  size;
        symvalue  rsize;

        if (n + 1 < nDataSymbols) /* not last one */
            rsize = section_symbols[n+1]->value  - sym->value;
        else
            rsize = bfd_section_size(object->bfdData, sec) - sym->value;

        size = rsize;
#if 0
        while ((size & 0x3) != 0)
            size++;
#endif
        
        if (size != 0) {
#if 0
            fprintf(stderr, "sym=%s, rsize=%ld, size=%ld\n",
                    name, rsize, size);
            fprintf(stdout, ".lcomm %s_%s.2,%ld\n", name, filename, size);
#endif
        }
        bss_read += size;
    }

    MEM_FREE(section_symbols);
#endif

#if 0
    while ((bss_size & 0x3) != 0) /* align */
        bss_size++;
#endif

    *total_bss += bss_size;
    *total_data += data_size;
    *comp_bss += bss_read;

    MEM_FREE(bfdSymbols);
    MEM_FREE(data_data);

    return 0;
}

int
segments_write_data(SegmentList* pSegments, FILE* fp,
                    bfd_size_type* maxData,
                    bfd_size_type* maxBss)
{
    SegmentList* segments = pSegments;
    bfd_size_type total_bss_size = 0;
    bfd_size_type total_data_size = 0;

    *maxData = 0;
    *maxBss = 0;
    
    for (; segments != NULL; segments = segments->_next) {
        Segment* segment = SegmentListGetSegment(segments);
        ObjectList* objects = segment->objects;
        int segmentIndex = segmentlist_get_segment_index(pSegments, segment);
        bfd_size_type segment_bss_size = 0;
        bfd_size_type segment_bss_pack = 0;
        bfd_size_type segment_data_size = 0;
        bfd_size_type pack;

        fprintf(fp, "| segment #%d\n", segmentIndex);

#define OBJECT_ALIGN  0x4
#define SEGMENT_BEGIN_ALIGN 0x4
#define SEGMENT_END_ALIGN   0x4
        
        /*
         * Pre segment packing. This ensures segment data is aligned.
         */
        pack = total_bss_size & (SEGMENT_BEGIN_ALIGN-1);
        if (pack != 0) {
            bfd_size_type i;
            pack = SEGMENT_BEGIN_ALIGN - pack;
            for (i = 0; i < pack; i++) {
                fprintf(fp,
                        ".lcomm bss_segment_%d_begin_padding_%03ld,1\n",
                        segmentIndex, i);
            }
        }
        
        /*
         * Segment data should now be aligned
         */
        segment->dataBegin = ftell(fp);
        
        for (; objects != NULL; objects = objects->_next) {
            Object* object = ObjectListGetObject(objects);
            bfd_size_type object_bss_size = 0;
            bfd_size_type object_data_size = 0;
            bfd_size_type dont_care;

            if (object_is_undefined(object))
                continue;
            
            if (object_write_data(object, fp,
                                  &object_data_size, &object_bss_size,
                                  &dont_care, &dont_care) == -1) {
                return -1;
            }
            
            segment_bss_size += object_bss_size;
            segment_data_size += object_data_size;

            /*
             * Object packing will done by the linker for data in
             * the segment, for data out of the segment, we just increment.
             * the segment total padding.
             */
            pack = (total_bss_size + segment_bss_size + segment_bss_pack) &
                (OBJECT_ALIGN-1);
            if (pack != 0) {
                pack = OBJECT_ALIGN - pack;
                segment_bss_size += pack;
#if 0
                fprintf(stderr, "segment #%d, object pack=%ld\n",
                        segmentIndex, pack);
#endif
            }
            
        }

#if 0
        fprintf(stderr,
                "segment #%d: bss = %ld, data = %ld\n",
                segmentIndex,
                segment_bss_size, segment_data_size);
#endif

        /* write out bss for the segment */
        if (segment_bss_size > 0) {
            bfd_size_type i;
            for (i = 0; i < segment_bss_size; i++) {
                fprintf(fp,
                        ".lcomm bss_segment_%d_data_%03ld,1\n",
                        segmentIndex, i);
            }
        }
        segment->dataEnd = ftell(fp);

        pack = (total_bss_size + segment_bss_size + segment_bss_pack) &
            (SEGMENT_END_ALIGN-1);
        if (pack != 0) {
            bfd_size_type i;
            pack = SEGMENT_END_ALIGN - pack;
            for (i = 0; i < pack; i++) {
                fprintf(fp,
                        ".lcomm bss_segment_%d_end_padding_%03ld,1\n",
                        segmentIndex, i);
            }
            segment_bss_pack += pack;
        }

        segment->bssSize = segment_bss_size + segment_bss_pack;
        segment->dataSize = segment_data_size;

        if (segment->bssSize > *maxBss)
            *maxBss = segment->bssSize;
        if (segment->dataSize > *maxData)
            *maxData = segment->dataSize;

        total_bss_size += segment->bssSize;
        total_data_size += segment->dataSize;
    }
    
    return ftell(fp);
}

char*
make_tmpnam(char* buf,
            const char* type, unsigned segmentIndex, const char* suffix)
{
    sprintf(buf, "%s%04d%s", type, segmentIndex, suffix);
    return buf;
}

static int
do_command(char** args, unsigned verbosity)
{
	int status;
	pid_t child_pid;
	char* file = args[0];

    if (verbosity) {
        int i;
        for (i = 0; args[i] != NULL; i++) {
            if (i != 0)
                fputc(' ', stdout);
            fputs(args[i], stdout);
        }
        fputc('\n', stdout);
    }

	if ((child_pid = fork()) == -1) {
		fprintf(stderr, "could not fork: ");
		perror(NULL);
		return -1;
	}

	if (child_pid == 0) { /* i am the child */
		if (execvp(file, args) == -1) {
			fprintf(stderr, "could not exec %s: ", file);
			perror(NULL);
			exit(3);
		}
		/*NOTREACHED*/
	}

	if (waitpid(child_pid, &status, 0) == -1) {
		fprintf(stderr, "wait on %s failed: ", file);
		perror(NULL);
		return -1;
	}

	return WEXITSTATUS(status);
}

static int
copy_file(FILE* ofp, FILE* ifp,
          long startPosn, long endPosn,
          long dataPad, long bssPad)
{
    long toGo = endPosn - startPosn;

    if (fseek(ifp, startPosn, SEEK_SET) == -1)
        return -1;

    while (toGo > 0) {
        int c = getc(ifp);

        if (c == EOF)
            return -1;

        putc(c, ofp);

        toGo--;
    }

    if (dataPad > 0) {
        fprintf(ofp, ".end_of_data_padding:\n");
        while (dataPad > 0) {
            fprintf(ofp, "    .byte 0\n");
            dataPad--;
        }
    }

    if (bssPad > 0) {
        fprintf(ofp, ".lcomm end_of_bss_padding,%ld\n", bssPad);
    }

    return 0;
}

#define BUFSIZE 256

static char*
extract_archive_object(char* filename, bfd* abfd)
{
    struct stat buf;
    long        size;
    FILE*       fp;
    char        copy_buf[BUFSIZE];
    int         ncopied;

    if (bfd_stat_arch_elt(abfd, &buf) != 0) {
        fprintf(stderr, "bdf internal stat error for %s", filename);
        return NULL;
    }
    size = buf.st_size;

    fp = fopen(filename, "wb");
    if (!fp) {
        fprintf(stderr, "could not open tmp file %s", filename);
        return NULL;
    }

    bfd_seek(abfd, 0, SEEK_SET);

    ncopied = 0;
    while (ncopied < size) {
        int nread;
        int tocopy = size - ncopied;

        if (tocopy > BUFSIZE)
            tocopy = BUFSIZE;

        nread = bfd_read(copy_buf, 1, tocopy, abfd);
        if (nread != tocopy) {
            fprintf(stderr, "bdf internal read error for %s", filename);
            fclose(fp);
            unlink(filename);
            return NULL;
        }

        fwrite(copy_buf, 1, nread, fp);
        ncopied += tocopy;
    }

    fclose(fp);

    return filename;
}

static int
make_segment_object(EmitCtx* eCtx,
                    Segment* segment, SegmentList* segmentList,
                    char* object_filename, FILE* dataFp, long dataFpSize,
                    bfd_size_type maxData, bfd_size_type maxBss,
                    char* comm_filename)
{
    char  exports_filename_buf[MAXPATHLEN];
    char  surrogates_filename_buf[MAXPATHLEN];
    char  data_begin_filename_buf[MAXPATHLEN];
    char  data_end_filename_buf[MAXPATHLEN];
    char  tmp_object_filename_buf[MAXPATHLEN];
    char* exports_filename = NULL;
    char* surrogates_filename = NULL;
    char* data_begin_filename = NULL;
    char* data_end_filename = NULL;
    int   return_value = 0;
    int   segmentIndex = segmentlist_get_segment_index(segmentList, segment);
    char* args[64]; /* hope that enough */
    char* tmps[64]; /* hope that enough */
    int   nargs;
    int   ntmps = 0;
    ObjectList* objectL;
    FILE* fp;

    /*
     * Make filenames.
     */
    make_tmpnam(exports_filename_buf, "e", segmentIndex, ".s");
    make_tmpnam(surrogates_filename_buf, "s", segmentIndex, ".s");
    make_tmpnam(data_begin_filename_buf, "db", segmentIndex, ".s");
    make_tmpnam(data_end_filename_buf, "de", segmentIndex, ".s");

    /*
     * Make exports file: e0000.s
     */
    exports_filename = exports_filename_buf;
    fp = fopen(exports_filename, "wb");
    if (!fp) {
        fprintf(stderr, "cannot open tmp file %s\n", exports_filename);
        return_value = -1;
        goto cleanup;
    }
    segment_write_jmptable(fp, segment, segmentIndex);
    fclose(fp);

    /*
     * Make surrogates file: s0000.s
     *
     * Always need to write out the constants with segment zero,
     * even if there are no surrogates (ie there is only one segment).
     */
    if (segment->surrogateSymbols != NULL || segmentIndex == 0) {
        surrogates_filename = surrogates_filename_buf;
        fp = fopen(surrogates_filename, "wb");

        if (fp == NULL) {
            fprintf(stderr, "cannot open tmp file %s\n", surrogates_filename);
            return_value = -1;
            goto cleanup;
        }

        if (segment->surrogateSymbols != NULL) {
            segment_write_surrogate_calls(fp, segment, segmentList, eCtx->cid);
        }
        
        if (segmentIndex == 0) {
            unsigned nSegments = segment_list_get_nsegments(segmentList);
            
            fprintf(fp,
                    ".global MultilinkSegmentNJmpTables\n"
                    "MultilinkSegmentNJmpTables:\n"
                    "    .word %d\n"
                    ".global MultilinkApplicationId\n"
                    "MultilinkApplicationId:\n"
                    "    .long 0x%lx\n",
                    nSegments,
                    eCtx->cid);
        }
        
        fclose(fp);
    }

    /*
     * Make data begin file: db0000.s
     */
    data_begin_filename = data_begin_filename_buf;
    fp = fopen(data_begin_filename, "wb");
    if (!fp) {
        fprintf(stderr, "cannot open tmp file %s\n", data_begin_filename);
        return_value = -1;
        goto cleanup;
    }
    copy_file(fp, dataFp, 0, segment->dataBegin, 0, 0);
    fclose(fp);

    /*
     * Make date end file: de0000.s
     */
    data_end_filename = data_end_filename_buf;
    fp = fopen(data_end_filename, "wb");
    if (!fp) {
        fprintf(stderr, "cannot open tmp file %s\n", data_end_filename);
        return_value = -1;
        goto cleanup;
    }
    copy_file(fp, dataFp, segment->dataEnd, dataFpSize, 0, 0);
    
    /*
     * Write out at very end, to make sure MultilinkSegmentJmpTables
     * is the highest bss symbol.
     */
    fputs("    .comm MultilinkSegmentJmpTables,4\n", fp);
    fclose(fp);
     
    /*
     * Build the compile command.
     */
    nargs = 0;
    args[nargs++] = palmGcc;
    if (eCtx->verbosity > 1) {
        args[nargs++] = "-v";
    }
    if (eCtx->minusg) {
        args[nargs++] = "-g";
    }
    args[nargs++] = "-o";
    args[nargs++] = object_filename;

#if 0
    args[nargs++] = "-Tbss";
    args[nargs++] = "100";
#endif

    if (segmentIndex == 0) {
        char* crt_file = NULL;
        
        if (eCtx->targetType == TARGET_APP) {
            if (eCtx->minusg) {
                /* debug start() file */
                crt_file = LIBFILE_GCRT0;
            } else {
                /* start() file */
                crt_file = LIBFILE_CRT0;
            }
        } else if (eCtx->targetType == TARGET_SYSLIB) {
            if (eCtx->minusg) {
                /* debug start() file */
                crt_file = LIBFILE_SLGCRT0;
            } else {
                /* start() file */
                crt_file = LIBFILE_SLCRT0;
            }
        }
        
        if (crt_file != NULL)
            args[nargs++] = get_library_filename(crt_file);

        if (eCtx->targetType == TARGET_SYSLIB) {
            args[nargs++] = get_library_filename(LIBFILE_DATA);
        } 

        args[nargs++] = exports_filename; /* like to have this first */
        args[nargs++] = get_library_filename(LIBFILE_LOAD);
        args[nargs++] = get_library_filename(LIBFILE_RLOC);
        args[nargs++] = get_library_filename(LIBFILE_GET);
    } else {
        args[nargs++] = "-shared";
        args[nargs++] = exports_filename; /* like to have this first */
        if (globalsInA4)
            args[nargs++] = get_library_filename(LIBFILE_GET);
    }

    args[nargs++] = comm_filename;        /* global uninitialised */
    args[nargs++] = data_begin_filename;  /* global initialised */
    if (surrogates_filename != NULL)
        args[nargs++] = surrogates_filename;

    /*
     * Add objects.
     */
    for (objectL = segment->objects; objectL!=NULL; objectL = objectL->_next){
        Object* object = ObjectListGetObject(objectL);
        if (object_is_undefined(object))
            continue;
        if ((object->flags & OBJECT_IN_ARCHIVE) != 0) {
            char* t = tmp_object_filename_buf;
            strcpy(t, "_");
            strcat(t, object->name);
            if (eCtx->verbosity > 1) {
                printf("extracting %s:%s to %s\n",
                       object->bfdData->my_archive->filename,
                       object->bfdData->filename,
                       t);
            }
            t = extract_archive_object(t, object->bfdData);
            if (t == NULL) {
                return_value = -1;
                goto cleanup;
            }
            t = strdup(t);
            args[nargs++] = t;
            tmps[ntmps++] = t;
        } else {
            args[nargs++] = object->name;
        }
    }
    args[nargs++] = data_end_filename; /* global initialised */

    /*
     * Libs
     */
    args[nargs++] = "-nostdlib";
    args[nargs] = NULL;

    /*
     * Run the gcc command
     */
    return_value = do_command(args, eCtx->verbosity);
    if (return_value != 0) {
        fprintf(stderr, "compile failed for segment %d\n", segmentIndex);
        return_value = -1;
        goto cleanup;
    }

 cleanup:
    if (!eCtx->leavetmp) {
        int n;

        if (exports_filename != NULL)
            unlink(exports_filename);
        if (surrogates_filename != NULL)
            unlink(surrogates_filename);
        if (data_begin_filename != NULL)
            unlink(data_begin_filename);
        if (data_end_filename != NULL)
            unlink(data_end_filename);

        for (n = 0; n < ntmps; n++) {
            unlink(tmps[n]);
            free(tmps[n]);
        }
    }
    
    return return_value;
}

static int
make_comm_object(char* object_name, SymbolTable* symbols, int verbosity,
                 int leavetmp, int minusg)
{
    char* comm_filename = "comm.s";
    FILE* fp;
    int   rv;
    char* args[64];
    int nargs;

    fp = fopen(comm_filename, "wb");

    rv = write_comm_section(fp, symbols); 
    
    fclose(fp);

    if (rv != -1) {
    
        nargs = 0;
        args[nargs++] = palmGcc;
        if (minusg) {
            args[nargs++] = "-g";
        }
        args[nargs++] = "-o";
        args[nargs++] = object_name;
        args[nargs++] = "-c";
        args[nargs++] = comm_filename;
        args[nargs] = NULL;

        if (do_command(args, verbosity) == -1) {
            fprintf(stderr,
                    "could not build common data object: %s\n",
                    object_name);
            rv = -1;
        }
    }

    if (!leavetmp) {
        unlink(comm_filename);
    }

    return rv;
}

int
segment_get_data_from_object(Segment* segment, char* object_filename)
{
    bfd* bfdData;
    asection* bss_section;
    bfd_size_type bss_size;
    asection* data_section;
    bfd_size_type data_size;
    unsigned char* data_data;

    if (!(bfdData = bfd_openr(object_filename, 0))) {
        fprintf(stderr, "Can not open %s\n", object_filename);
        return -1;
    }

    if (bfd_check_format(bfdData, bfd_object) == 0) {
        fprintf(stderr, "%s is not an object file\n", object_filename);
        return -1;
    }

    bss_section = bfd_get_section_by_name(bfdData, ".bss");
    bss_size = bfd_section_size(bfdData, bss_section);

    data_section = bfd_get_section_by_name(bfdData, ".data");
#if 1
    data_size = bss_section->vma - data_section->vma;
#else
    data_size = bfd_section_size(bfdData, data_section);
#endif

#if 0
    fprintf(stderr,
            "bss_section->vma - data_section->vma = %d\n"
            "bfd_section_size(bfdData, data_section) = %d\n",
            bss_section->vma - data_section->vma,
            bfd_section_size(bfdData, data_section));
#endif
    
    /*
     * Allocate enough memory to cover any alignment, and make sure
     * the padding at the end is NULed out. That way, when we do the
     * big data segment comparison later the padding won't mismatch.
     */
    data_data = MEM_CALLOC(data_section->_raw_size + 32, sizeof(char));
    if (bfd_get_section_contents(bfdData,
                                 data_section,
                                 data_data,
                                 0,
                                 data_section->_raw_size) == false) {
        fprintf(stderr, "error reading data section for %s\n",object_filename);
        return -1;
    }

#if 0
    /* this is total crap. the segment-> sizes are total for the app */
    if (segment->bssSize != bss_size) {
        fprintf(stderr,
                "WARNING: computed bss size (%ld) != found size (%ld) for %s\n",
                segment->bssSize,
                bss_size,
                object_filename);
    }
    if (segment->dataSize != data_size) {
        fprintf(stderr,
                "WARNING: computed data size (%ld) != found size (%ld) for %s\n",
                segment->dataSize,
                data_size,
                object_filename);
    }
#endif
    
    segment->dataData = data_data;
    segment->dataSize = data_size;
    segment->bssSize = bss_size;

    bfd_close_all_done(bfdData);

    return 0;
}

int
segments_merge_data_resource(SegmentList* pSegmentList, int verbosity)
{
    SegmentList* segmentList = pSegmentList;
    Segment* segment;
    bfd_size_type bss_size;
    bfd_size_type data_size;
    unsigned char* data_data;
    unsigned segmentn = 0;
    unsigned char* dataEnd;
    unsigned char* p;
    unsigned char* q;
    int rv = 0;
    
    if (pSegmentList == NULL)
        return -1;

    segment = SegmentListGetSegment(segmentList);
    bss_size = segment->bssSize;
    data_size = segment->dataSize;
    data_data = segment->dataData;
    dataEnd = &data_data[data_size];

    if (verbosity > 1) {
        for (p = data_data; p < dataEnd; p++) {
            if (*p != 0) {
                fprintf(stderr,
                        "merging initialized data from #%d,%d:0x%02x\n",
                        segmentn, (p - data_data), *p);
            }
        }
    }

    for (segmentList = segmentList->_next, segmentn = 1;
         segmentList != NULL;
         segmentList = segmentList->_next, segmentn++) {
        segment = SegmentListGetSegment(segmentList);

        if (segment->bssSize != bss_size) {
            fprintf(stderr,
                    "bss size for segment #%d does not match #0\n",
                    segmentn);
            rv = -1;
        }

        if (segment->dataSize != data_size) {
            fprintf(stderr,
                    "data size for segment #%d does not match #0\n",
                    segmentn);
            rv = -1;
        }

        for (p = data_data, q = segment->dataData; p < dataEnd; p++, q++) {
            if (*p != 0 && *q != 0 && *p != *q) {
                fprintf(stderr,
                        "mismatched data found in segment #%d,0x%02x:"
                        "0x%02x<->0x%02x\n",
                        segmentn, (p - data_data), *p, *q);
            } else if (*p == 0 && *q != 0) {
                if (verbosity > 1) {
                    fprintf(stderr,
                            "merging initialized data from #%d,%d:0x%02x\n",
                            segmentn, (p - data_data), *q);
                }                
                *p = *q;
            }
        }
        MEM_FREE(segment->dataData);
        segment->dataData = NULL;
    }

    return rv;
}

static int
segments_apply_relocation_patches(unsigned char* data,
                                  unsigned data_len,
                                  List* relocations)
{
    List* l;

    for (l = relocations; l != NULL; l = l->_next) {
        Relocation*    r = RelocationListGetRelocation(l);
        unsigned long* loc;

        if (r->offset > data_len) {
            fprintf(stderr, "relocation offset outside data\n");
            return -1;
        }
        loc = (unsigned long*)((char*)data + r->offset);
        *loc = htonl(ntohl(*loc) - r->adjustment);
    }
    return 0;
}

static int
make_segment_objects(EmitCtx*     eCtx,
                     SegmentList* pSegmentList,
                     SymbolTable* symbols)
{
    SegmentList* segmentList = pSegmentList;
    char* comm_filename = "comm.o";
    FILE* dataFp;
    long  dataFpSize;
    bfd_size_type maxData = 0;
    bfd_size_type maxBss = 0;
    int rv;
    Segment* segment;
    unsigned char* resource_data;
    bfd_size_type  resource_data_len;
    int segmentIndex = 0;
    char foo_filename[MAXPATHLEN];
    char* data_filename = "data.s";
    const char* coff_prefix;

    if (make_comm_object(comm_filename, symbols,
                         eCtx->verbosity,
                         eCtx->leavetmp,
                         eCtx->minusg) == -1) {
        return -1;
    }

    dataFp = fopen(data_filename, "wb+");
    if (dataFp == NULL) {
        fprintf(stderr, "could not open 'data.s' for writing\n");
        return -1;
    }

    dataFpSize = segments_write_data(segmentList, dataFp,
                                     &maxData, &maxBss);
    if (dataFpSize == -1) {
        fprintf(stderr, "writing data define file failed\n");
        return -1;
    }

    if (eCtx->basename != NULL)
        coff_prefix = eCtx->basename;
    else
        coff_prefix = DEFAULT_AOUT_PREFIX;

    for (; segmentList != NULL;
         segmentList = segmentList->_next, segmentIndex++) {

        segment = SegmentListGetSegment(segmentList);

        if (eCtx->verbosity)
            printf("making segment #%d\n", segmentIndex);

        make_tmpnam(foo_filename, coff_prefix, segmentIndex,
                    DEFAULT_AOUT_SUFFIX);

        rv = make_segment_object(eCtx,
                                 segment,
                                 pSegmentList,
                                 foo_filename,
                                 dataFp,
                                 dataFpSize,
                                 maxData,
                                 maxBss,
                                 comm_filename);
        if (rv == -1)
            return -1;

        rv = make_code_resource_files(eCtx,
                                      foo_filename,
                                      segmentIndex, eCtx->targetType);
        if (rv == -1)
            return -1;

        rv = segment_get_data_from_object(segment, foo_filename);
        if (rv == -1)
            return -1;
    }

    fclose(dataFp);
    if (!eCtx->leavetmp) {
        unlink(data_filename);
    }

    rv = segments_merge_data_resource(pSegmentList, eCtx->verbosity);
    if (rv == -1)
        return -1;

    segment = SegmentListGetSegment(pSegmentList);
    rv = segments_apply_relocation_patches(segment->dataData,
                                           segment->dataSize,
                                           eCtx->relocations);
    if (rv == -1)
        return -1;

    resource_data = compress_data(segment->dataData,
                                  segment->dataSize,
                                  segment->bssSize,
                                  &resource_data_len);

    EmitResource(eCtx, "data", 0, resource_data, resource_data_len);

    if (!eCtx->leavetmp) {
        unlink(comm_filename);
    }
    
    return 0;
}

static int
make_gdb_loadsegment_file(char*        filename,
                          char*        basename,
                          SegmentList* segments,
                          int          verbosity)
{
    unsigned nSegments = segment_list_get_nsegments(segments);
    unsigned n;
    FILE*    fp;
    char*    coff_prefix;

    if (verbosity)
        printf("making gdb-script: %s\n", filename);

    fp = fopen(filename, "w");
    if (!fp) {
        fprintf(stderr, "could not open file: %s\n", filename);
        return -1;
    }

    fprintf(fp,
            "# Generated by multilink -gdb-script option\n"
            "define load-segments\n"
            "  set $jmptable = *((long**)($a5-4))\n"
            "  echo loading segments\n");

    if (basename != NULL)
        coff_prefix = basename;
    else
        coff_prefix = DEFAULT_AOUT_PREFIX;

    for (n = 1; n < nSegments; n++) {
        fprintf(fp,
                "  echo .\n"
                "  add-symbol-file %s%04d"
                DEFAULT_AOUT_SUFFIX
                " *(long*)(((long)$jmptable)+%d)\n",
                coff_prefix,
                n,
                (n*4));
    }

    fprintf(fp,
            "  echo done\\n\n"
            "end\n");

    if (!basename)
        basename = DEFAULT_AOUT_PREFIX DEFAULT_AOUT_SUFFIX;

    fprintf(fp,
            "document load-segments\n"
            "  load-segments command for %s (%d segments)\n"
            "  Use the load-segments command to load the non-zeroth segments\n"
            "  in a multi-segment application. This allows you do debug code\n"
            "  in any segment.\n"
            "end\n",
            basename,
            nSegments);

    fclose(fp);

    return 0;
}

static unsigned long
catoi(char* s)
{
    long foo;

    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        foo = strtol(&s[2], NULL, 16);
    } else if (isalpha(s[0]) && strlen(s) > 3) { /* Mac long */
        memcpy(&foo, s, 4);
        foo = ntohl(foo);
    } else { /* whatever??? */
        foo = atoi(s);
    }
    return foo;
}

static unsigned
katoi(char* s)
{
    long foo;
    char* p;

    foo = strtol(s, &p, 10);

    if (p != NULL && (*p == 'k' || *p == 'K'))
        foo *= 1024;

    return (unsigned)foo;
}

#define MAXSEGMENTSIZE (30*1024) /* 32k - fudge */

static void
usage()
{
    fprintf(stderr,
            "usage: multilink [options] <objects> <libs.a> <-llibs>\n"
            "options are:\n"
            "-help             - print this usage information\n"
            "-version          - print the version number\n"
            "-g                - use debug versions of startup files\n"
            "-nostdlib         - do not link standard libs into target\n"
            "-stdlib           - link standard libs (-lgcc, -lc) into target\n"
            "-debug            - output random debugging noise\n"
            "-leavetmp         - leave generated glue files around\n"
            "-verbose          - be more verbose (n levels)\n"
            "-syslib           - target will be a SysLib .prc (default is app)"
            "\n"
            "-segmentsize n[k] - define maximum segment size (default is 30k)"
            "\n"
            "-basename name    - resulting files will be TYPEXXXX.<name>.grc\n"
            "-segmentmap file  - explicitly define segments (default is auto)"
            "\n"
            "-libdir dir       - directory to look for startup files\n"
            "-fid fid          - FtrID jmptables will be installed using\n"
            "-unused           - dump unused symbols\n"
            "-use-compiler cc  - specify compiler to use "
            "(default is " MULTILINK_CC ")\n"
            "-a4-globals       - generate runtime code using a4-rel globals"
#if (DEFAULT_A4_GLOBALS == 1)
            " (default)"
#endif
            "\n"
            "-a5-globals       - generate runtime code using a5-rel globals"
#if (DEFAULT_A4_GLOBALS == 0)
            " (default)"
#endif
            "\n"
            "-relocation-old   - assume gcc 2.7 relocation information"
#if (DEFAULT_RELOCATION_OLD == 1)
            " (default)"
#endif
            "\n"
            "-relocation-new   - assume gcc 2.95 relocation information"
#if (DEFAULT_RELOCATION_OLD == 0)
            " (default)"
#endif
            "\n"
            "-gdb-script file  - generate script with useful gdb commands\n"
            "-stdlibpath path  - path to search for stdlib files\n"
            "environment variables:\n"
            "MULTILINK_LIBDIR  - directory to look for startup files\n"
            "MULTILINK_STDLIBPATH - path to search for stdlib files\n"
            );
}


static int
process_filename(World* theWorld,
                 LibdirList* userlibdirList, LibdirList* syslibdirList,
                 const char* fname)
{
    bfd* bf;
    bfd* archive;

    if (fname[0] == '-' && fname[1] == 'l') {
        char  namebuf[MAXPATHLEN];
        char* lname;

        lname = (char*)library_lookup(namebuf, userlibdirList, &fname[2]);
        if (!lname)
            lname = (char*)library_lookup(namebuf, syslibdirList, &fname[2]);
        if (!lname) {
            fprintf(stderr,"Unable to find library: %s\n", fname);
            return -1;
        }

        fname = strdup(lname);
    }
        
    bf = bfd_openr(fname, NULL);

    if (bf == NULL) {
        fprintf(stderr,"Can't open %s\n", fname);
        return -1;
    }

    if (bfd_check_format(bf, bfd_object)) { /* it's an object */
        process_object(theWorld, bf);

#if 0
        bfd_close_all_done(archive);
#endif
    } else if (bfd_check_format(bf, bfd_archive)) { /* it's an archive */
        archive = bf;
        bf = NULL;

        while ((bf = bfd_openr_next_archived_file(archive, bf)) != NULL) {
            Object* object;
            if (!bfd_check_format(bf, bfd_object)) {
                fprintf(stderr,
                        "Found non-object in archive %s\n",
                        fname);
                return -1;
            }
            object = process_object(theWorld, bf);
            if (!object)
                return -1;
            object->flags |= OBJECT_IN_ARCHIVE;
        }
#if 0
        bfd_close_all_done(archive);
#endif
			
    } else { /* don't know */
        fprintf(stderr,
                "File %s is not an object or archive file\n", fname);
        return -1;
    }

    return 0;
}

int
main(int argc, char** argv)
{
    int  i;
    char* fname;
    World theWorld;
    SegmentList* segments;
    int verbosity = 0;
    unsigned long cid = 0x53427362UL; /*BSbs*/
    unsigned char* target_basename = NULL;
    int rv;
    unsigned maxsegmentsize = MAXSEGMENTSIZE;
    char* segment_mapfile = NULL;
    int debug = 0;
    int leavetmp = 0;
    int minusg = 0;
    int stdlib = 0;
    TargetType targetType = TARGET_APP;
    int dumpUnused = 0;
    EmitCtx  eCtxBuf;
    EmitCtx* eCtx;
    char*    gdb_scriptname = NULL;
    LibdirList* userlibdirList = NULL;
    LibdirList* stdlibdirList = NULL;
    char*       stdlib_path = MULTILINK_STDLIB;
    char*       tmp_env;

    memset(&theWorld, 0, sizeof(World));

    theWorld.symbols = SymbolTableNew(1000);
    init_symbols(theWorld.symbols);

    bfd_init(); /* init bfd */

    if (argc < 2) {
        usage();
        return 2;
    }

    palmGcc = MULTILINK_CC;
    globalsInA4 = DEFAULT_A4_GLOBALS;
    relocateIs05 = DEFAULT_RELOCATION_OLD;

    tmp_env = getenv("MULTILINK_STDLIBPATH");
    if (tmp_env != NULL)
        stdlib_path = tmp_env;

    library_add_paths(&stdlibdirList, stdlib_path);

    for (i = 1; i < argc; i++) {

        fname = argv[i];

        if (strcmp(fname, "-help") == 0) {
            usage();
            return 2;
        }
        
        if (strcmp(fname, "-use-compiler") == 0) {
            palmGcc = argv[i+1];
            i++;
            continue;
        }
        
        if (strcmp(fname, "-a4-globals") == 0) {
            globalsInA4 = 1;
            continue;
        }
        
        if (strcmp(fname, "-a5-globals") == 0) {
            globalsInA4 = 0;
            continue;
        }
        
        if (strcmp(fname, "-relocation-old") == 0) {
            relocateIs05 = 1;
            continue;
        }
        
        if (strcmp(fname, "-relocation-new") == 0) {
            relocateIs05 = 0;
            continue;
        }
        
        if (strcmp(fname, "-g") == 0) {
            minusg++;
            continue;
        }
        
        if (strcmp(fname, "-deadstrip") == 0) {
            continue;
        }
        
        if (strcmp(fname, "-nostdlib") == 0) {
            stdlib = 0;
            continue;
        }
        
        if (strcmp(fname, "-stdlib") == 0) {
            stdlib = 1;
            continue;
        }
        
        if (strcmp(fname, "-unused") == 0) {
            dumpUnused++;
            continue;
        }
        
        if (strcmp(fname, "-debug") == 0) {
            debug++;
            continue;
        }

        if (strcmp(fname, "-leavetmp") == 0) {
            leavetmp++;
            continue;
        }

        if (strcmp(fname, "-verbose") == 0) {
            verbosity++;
            continue;
        }

        if (strcmp(fname, "-syslib") == 0) {
            targetType = TARGET_SYSLIB;
            continue;
        }

        if (strcmp(fname, "-segmentsize") == 0) {
            maxsegmentsize = katoi(argv[i+1]);
            i++;
            continue;
        }

        if (strcmp(fname, "-basename") == 0) {
            target_basename = argv[i+1];
            i++;
            continue;
        }

        if (strcmp(fname, "-segmentmap") == 0) {
            i++;
            segment_mapfile = argv[i];
            continue;
        }

        if (strcmp(fname, "-gdb-script") == 0) {
            i++;
            gdb_scriptname = argv[i];
            continue;
        }

        if (strcmp(fname, "-libdir") == 0) {
            i++;
            libDirVar = strdup(argv[i]);
            continue;
        }

        if (strcmp(fname, "-fid") == 0) {
            i++;
            cid = catoi(argv[i]);
            continue;
        }

        if (strcmp(fname, "-V") == 0 || strcmp(fname, "-version") == 0) {
            printf("%ld.%ld",
                   MULTILINK_VERSION_MAJOR, MULTILINK_VERSION_MINOR);
            if (MULTILINK_VERSION_MICRO != 0)
                printf(".%ld", MULTILINK_VERSION_MICRO);
            printf("\n");
            return 0;
        }

        if (strcmp(fname, "-stdlibpath") == 0) {
            stdlibdirList = NULL;
            i++;
            library_add_paths(&stdlibdirList, argv[i]);
            continue;
        }

        if (fname[0] == '-' && fname[1] == 'L') {
            const char* libname = &fname[2];
            ListAppend(&userlibdirList, LibraryListNew(libname));
            continue;
        }

        rv = process_filename(&theWorld, userlibdirList, stdlibdirList, fname);
        if (rv == -1)
            return 1;
    }

    if (stdlib) {
        rv = process_filename(&theWorld,
                              userlibdirList, stdlibdirList,
                              "-lgcc");
        if (rv == -1)
            return 1;
        rv = process_filename(&theWorld,
                              userlibdirList, stdlibdirList,
                              "-lc");
        if (rv == -1)
            return 1;
    }

    if (debug >= 1) {
        dump_symbol_info(theWorld.symbols);
        dump_object_info(theWorld.objects);
    }

    remove_unused_objects(&theWorld.objects,
                          theWorld.symbols,
                          targetType, verbosity);

    if (segment_mapfile != NULL) {
        FILE* fp;

        fp = fopen(segment_mapfile, "r"); /* in text mode */
        if (fp == NULL) {
            fprintf(stderr,
                    "Could not open segment mapfile '%s'\n",
                    segment_mapfile);
            return 1;
        }
        segments = make_segment_list_from_mapfile(fp,
                                                  theWorld.objects,
                                                  maxsegmentsize);
        fclose(fp);
    } else {
        segments = make_segment_list_simple(NULL,
                                            theWorld.objects,
                                            maxsegmentsize);
    }

    segments = segment_list_fixups(segments, theWorld.symbols, targetType);
    
    segments_make_jmptable_and_surrogates(segments, theWorld.symbols);

    if (debug >= 1) {
        dump_segments(segments);
    }

    if (dumpUnused > 0) {
        dump_unused_objects(theWorld.objects);
        dump_unused_symbols(theWorld.symbols);
    }

    eCtx = EmitCtxInit(&eCtxBuf,
                       target_basename,
                       cid,
                       verbosity,
                       leavetmp,
                       minusg,
                       targetType);
    
    rv = make_segment_objects(eCtx, segments, theWorld.symbols);
    if (rv == -1)
        return 1;

    if (gdb_scriptname != NULL) {
        make_gdb_loadsegment_file(gdb_scriptname,
                                  target_basename,
                                  segments,
                                  verbosity);
    }
    
    EmitCtxFinalize(eCtx);

    return 0;
}
