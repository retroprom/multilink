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
 * shlibcrt0.c - SysLib startup code.
 *
 * created: David Williams, djw@avantgo.com, March, 1999.
 */
#include "multilinkrt.h"

extern long data_start, bss_start;

/* Must be provided by shared library glue code. */
Err ShlibMain(UInt16 refNum, SysLibTblEntryPtr entryP);

register void* reg_a4 asm("%a4");
register void* reg_a5 asm("%a5");

Err
start(UInt16 refNum, SysLibTblEntryPtr entryP);

asm("
    .globl _shlib_start
    .globl start
_shlib_start:
start:
    bra.w __Startup__
");

/*static*/ Err
__Startup__(UInt16 refNum, SysLibTblEntryPtr entryP)
{
    unsigned** jmptables;
    void* save_a4;
    void* save_a5;
    void* globals;
    void* bss;
    unsigned segmentn;
    SysAppInfoType* appInfo;
    Err             rv;
    
#ifdef PALMOS_3_3_HACK
    DmOpenRef       dbR;

    /*
     * This is here because PalmOS 3.3 SysLibLoad() closes the library
     * database before calling SysLibInstall() - which calls us. Without
     * the database being open, we cannot get at the resources. Lossage.
     * This is bad enough, but because we cannot query our database
     * we can't determine our type, cid, or anything else. For now,
     * we have to hard code the type as 'libr' which means all
     * libraries will stop working if you change the type to something
     * else. This is no good...djw July/1999.
     */
    dbR = DmOpenDatabaseByTypeCreator('libr',
                                      MULTILINK_APPL_ID,
                                      dmModeReadOnly);
    if (!dbR) {
        ErrFatalDisplayIf(1, "Could not open library database");
        return sysErrLibNotFound;
    }
#endif /*PALMOS_3_3_HACK*/

    appInfo = *(SysAppInfoType**)reg_a5;

    /* load other code segments */
    jmptables = MultilinkLoadCodeSegments(MULTILINK_APPL_ID,
                                          MULTILINK_SEGMENT_JMPTABLE,
                                          MULTILINK_NJMPTABLES,
                                          0); /* don't reloc here */
    
    /*
     * load data,
     * do data relocations for each code segment.
     */
    globals = MultilinkLoadData();

    bss = (void*)((char*)globals + (&bss_start - &data_start));
    MultilinkRelocateDataRsrc(0, &start, globals, bss);

    for (segmentn = 1; segmentn < MULTILINK_NJMPTABLES; segmentn++) {
        UInt32 rsrcId = MULTILINK_JMPTABLES_SEGMENT_RSRC_ID(segmentn);
        MultilinkRelocateDataRsrc(rsrcId,
                                  (void*)jmptables[segmentn], globals, bss);
    }

    /*
     * Punch the code jmp tables into the data segment.
     */
    save_a4 = reg_a4;
    save_a5 = reg_a5;

    /*
     * Set the a4, a5 registers appropriately for the library.
     * Do some hacking on own globals, then call ShlibMain().
     */
    reg_a4 = globals;
    reg_a5 = globals;
    asm volatile ("add.l #edata,%a5");
    
    MultilinkSegmentJmpTables = jmptables;

    /* set up our globals as the default value for ShlibMain */
    entryP->globalsP = reg_a5;
    
    /*
     * Save the appInfo block in the new globals.
     */
    *((SysAppInfoType**)reg_a5) = appInfo;

    rv = ShlibMain(refNum, entryP);

    /* Put things back the way the caller had them */
    reg_a4 = save_a4;
    reg_a5 = save_a5;

#ifdef PALMOS_3_3_HACK
    DmCloseDatabase(dbR);
#endif /*PALMOS_3_3_HACK*/

    return rv;
}

void
_shlib_end(void* globals) /* entryP->globalsP */
{
    unsigned** jmptables;
    void* save_a4;
    void* save_a5;

    save_a4 = reg_a4;
    save_a5 = reg_a5;

    reg_a5 = globals;
    reg_a4 = globals;
    asm volatile ("sub.l #edata,%a4");
    globals = reg_a4;

    jmptables = MultilinkSegmentJmpTables;

    reg_a4 = save_a4;
    reg_a5 = save_a5;

    MultilinkUnloadCodeSegments(MULTILINK_APPL_ID,
                                jmptables,
                                MULTILINK_NJMPTABLES);

    MemPtrFree(globals);
}

