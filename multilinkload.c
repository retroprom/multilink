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
 * multilinkload.c - runtime support for loading code segments.
 *
 * created: David Williams, djw@avantgo.com, July, 1998.
 */
#include "multilinkrt.h"

#if 0
#define MULTILINK_MULTI_DB_SEGMENTS
#endif

typedef struct HandlePtrPair
{
    MemHandle handle;
    unsigned* ptr;
} HandlePtrPair;

static void
multilinkLoadSegment(HandlePtrPair* rv,
                     UInt32 creatorId, unsigned segmentn, Boolean rlocData)
{
    char      errmsg[80];
    MemHandle  codeSegment;
    unsigned* jmptable;
    UInt32 rsrcId = MULTILINK_JMPTABLES_SEGMENT_RSRC_ID(segmentn);
    UInt16  lockCount;

    /* 
     * The code segment is stored as a MULTILINK_CODE_RSRC_TYPE type resource
     * with rsrcId resource id.
     */
#ifndef MULTILINK_MULTI_DB_SEGMENTS
    codeSegment = DmGet1Resource(MULTILINK_CODE_RSRC_TYPE, rsrcId);
#else
    codeSegment = DmGetResource(MULTILINK_CODE_RSRC_TYPE, rsrcId);
    if (!codeSegment) {
        /*
         * Not in this prc, maybe in another one (a code fragment prc).
         * First get the name of the prc from a string in this resdb.
         * Then go find the db named that string. It will be a db with
         * our same creatorID, but the type will be 'code' instead of
         * 'appl'. But the, this might just all be fucked.
         */
        MemHandle nameHandle = DmGetResource('tSTR', rsrcId);
        
        if (nameHandle != 0) {
            char*   name = (char*)MemHandleLock(nameHandle);
            LocalID dbId = DmFindDatabase(0, name);

            if (dbId != 0) {
                DmOpenRef dbref = DmOpenDatabase(0, dbId, dmModeReadOnly);
                
                if (!codeSegment) {
                    StrCopy(errmsg, "Found but could not open database: ");
                    StrCopy(&errmsg[StrLen(errmsg)], name);
                    ErrFatalDisplayIf(1, errmsg);
                }
                codeSegment = DmGet1Resource(MULTILINK_CODE_RSRC_TYPE, rsrcId);
            }
            
            MemHandleUnlock(nameHandle);
            DmReleaseResource(nameHandle);
        }
    }
#endif /*MULTILINK_MULTI_DB_SEGMENTS*/
    if (!codeSegment) {
        StrCopy(errmsg, "Could not load code segment #");
        StrIToA(&errmsg[StrLen(errmsg)], rsrcId);
        ErrFatalDisplayIf(1, errmsg);
    }

    lockCount = MemHandleLockCount(codeSegment);

    /*
     * Jmptable is the front of the code segment.
     */
    jmptable = (unsigned*)MemHandleLock(codeSegment);
    if (!jmptable) {
        StrCopy(errmsg, "Could not lock code segment #");
        StrIToA(&errmsg[StrLen(errmsg)], rsrcId);
        ErrFatalDisplayIf(1, errmsg);
    }

    jmptable += 2; /* skip over exec magic */

    /*
     * Go do you relocation thing sonny.
     */
    if (rlocData) {
        extern long data_start, bss_start;
        MultilinkRelocateDataRsrc(rsrcId, jmptable, &data_start, &bss_start);
    }

    rv->ptr = jmptable;

    if (lockCount != memPtrLockCount) /* !ROM */
        rv->handle = codeSegment;
    else
        rv->handle = 0; /* don't unlock the handle if in ROM */
}

unsigned**
MultilinkLoadCodeSegments(UInt32 creatorId,
                          const void* code1jmptable,
                          const unsigned nsegments,
                          Boolean rlocData)
{
    Char      errmsg[80];
    unsigned** jmptables;
    Err err;
	UInt32 featureId;

	featureId = MULTILINK_JMPTABLES_FTR_ID(creatorId);
    err = FtrGet(creatorId, featureId, (UInt32 *)&jmptables);

    if (err != 0 || jmptables == NULL) {
        unsigned segmentn;
        /*
         * We'll store both the adjusted code entry point (see above)
         * and the code resource handle. The first is used as the redirect
         * for inter-segment calls. We need to keep the latter so we
         * can do cleanup at the end of the app.
         */
        jmptables = (unsigned**)MemPtrNew(sizeof(unsigned*) *
                                          (UInt32)((2 * nsegments) + 1));
		if (!jmptables) {
			StrCopy(errmsg, "Could not allocate memory for segment JmpTables");
			ErrFatalDisplayIf(1, errmsg);
		}

        jmptables[0] = (unsigned*)code1jmptable;
        jmptables[nsegments] = (unsigned*)0UL; /* don't know it */

        for (segmentn = 1; segmentn < nsegments; segmentn++) {
            HandlePtrPair pair;
            multilinkLoadSegment(&pair, creatorId, segmentn, rlocData);
            jmptables[segmentn] = pair.ptr;
            jmptables[segmentn+nsegments] = (unsigned*)pair.handle;
        }
        
        jmptables[2 * nsegments] = 0;

		MemPtrSetOwner(jmptables, 0);

		/* Set the Feature */
		FtrSet(creatorId, featureId, (UInt32)jmptables);
    }

    jmptables[2 * nsegments]++;

    return jmptables;
}

void
MultilinkUnloadCodeSegments(UInt32 creatorId,
                            unsigned** jmptables,
                            const unsigned nsegments)
{
	UInt32 featureId;
    unsigned n;
    
    jmptables[2 * nsegments]--;

    if (jmptables[2 * nsegments] == 0) { /* no users */

        featureId = MULTILINK_JMPTABLES_FTR_ID(creatorId);

        /* don't unload segment 0, let the OS do that */
        for (n = 1; n < nsegments; n++) {
            /* we actually only care about the handles now */
            MemHandle handle = (MemHandle)jmptables[n+nsegments];
            if (handle != 0) { /* don't unlock ROM handles */
                MemHandleUnlock(handle);
                DmReleaseResource(handle);
            }
        }

        FtrUnregister(creatorId, featureId);
        MemPtrFree(jmptables);
    }
}
