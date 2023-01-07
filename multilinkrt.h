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
 * multilinkrt.h - runtime support for multilink.
 *
 * created: David Williams, djw@avantgo.com, July, 1998.
 */
#ifndef __MULTILINKRT_H__
#define __MULTILINKRT_H__

/* Check for PalmOS */
#ifndef __palmos__
#error This header is intended for a PalmOS environment.
#endif

/* System files */
#ifdef MULTILINK_NEW_HEADERS

#include <PalmOS.h>

#define memPtrLockCount	15

#else

#include <Pilot.h>
#include <Common.h>
#include <System/SysAll.h>
#include <System/FeatureMgr.h>
#include <System/DataMgr.h>
#include <System/MemoryPrv.h>
#define NON_PORTABLE
#include <SystemPrv.h>
#include <UI/UIAll.h>

typedef signed char		Int8;
typedef signed short    Int16;	
typedef signed long		Int32;

typedef unsigned char	UInt8;
typedef unsigned short  UInt16;
typedef unsigned long   UInt32;

typedef VoidHand        MemHandle;
typedef Ptr             MemPtr;

#endif /*MULTILINK_NEW_HEADERS*/

#define MULTILINK_CODE_RSRC_TYPE 'code'
#define MULTILINK_DATA_RSRC_TYPE 'data' /* will go away with globals merging */

#define MULTILINK_JMPTABLES_FTR_ID(cid)        ((cid)+0x8000)
#define MULTILINK_JMPTABLES_SEGMENT_RSRC_ID(n) ((n)? (n)+0x8000: 0)

typedef struct {
    void* jmpTables;
    void* appInfo;
} MultilinkAppInfoAndJmpTable;

/*
 * These two guys are data in the text segment. They are declared as
 * functions, as that's the only way I know how to force other objects
 * to look for this guy in the text segment.
 *
 * TODO FIXME use section attributes
 */
extern void MultilinkSegmentJmpTable();   /* text: this segment's table */
extern void MultilinkSegmentNJmpTables(); /* text: number of jmptables  */
extern void MultilinkApplicationId();     /* creator id */
#define MULTILINK_NJMPTABLES         (*(unsigned*)MultilinkSegmentNJmpTables)
#define MULTILINK_SEGMENT_JMPTABLE   ((unsigned*)MultilinkSegmentJmpTable)
#define MULTILINK_APPL_ID            (*(long*)MultilinkApplicationId)

extern unsigned** MultilinkSegmentJmpTables; /* common: jmptable pointers  */

unsigned** MultilinkLoadCodeSegments(UInt32 creatorId,
                                     const void* code1jmptable,
                                     const unsigned nsegments,
                                     Boolean rlocData);
unsigned** MultilinkGetCodeSegments(UInt32 creatorId);
void       MultilinkRelocateData(UInt32 rsrcId, void* text_a);
void       MultilinkUnloadCodeSegments(UInt32 creatorId,
                                      unsigned**,
                                      const unsigned);
void*      MultilinkLoadData();
void       MultilinkRelocateDataRsrc(UInt32 rsrcId,
                                     void* text_a, void* data_a, void* bss_a);

#endif /*__MULTILINKRT_H__*/
