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
 * multilinkrloc.c - runtime support for relocating gcc generated data.
 *
 * created: David Williams, djw@avantgo.com, July, 1998.
 * largely based on code by Kresten Krab Thorup:
 *
 *  Pilot startup code for use with gcc.  This code was written 
 *  by Kresten Krab Thorup, and is in the public domain.
 *  It is *not* under the GPL or the GLPL, you can freely link it
 *  into your programs.
 */
#include "multilinkrt.h"
#include "multilinkrloc.h"

/*
 *  This function should be called from 
 */
void
MultilinkRelocateDataRsrc(UInt32 rsrcId,
                          void* text_a, void* data_a, void* bss_a)
{
    unsigned long text;
    unsigned long bss;
    unsigned long data;
    MemHandle     relocH;
    char*         relocPtr;
    pilot_reloc*  relocs;
    unsigned      count;
    unsigned      i;

    text = (unsigned long)text_a;
    bss  = (unsigned long)bss_a;
    data = (unsigned long)data_a;

    relocH = DmGet1Resource('rloc', rsrcId);
    if (relocH == 0)
        return;

    relocPtr = MemHandleLock(relocH);
    count = *(unsigned short*)relocPtr;
    relocs = (pilot_reloc*)(relocPtr + 2);

    for (i = 0; i < count; i++) {
        unsigned long* loc;
        unsigned       offset;

        ErrFatalDisplayIf((relocs[i].type != RELOC_ABS_32),
                          "unknown reloc.type");

        offset = relocs[i].offset;
        loc = (unsigned long*)((char*)data_a + offset);

        switch (relocs[i].section) {
        case TEXT_SECTION:
            *loc += text;
            break;

        case DATA_SECTION:
            *loc += data;
            break;

        case BSS_SECTION:
            *loc += bss;
            break;
	  
        default:
            ErrDisplay("Unknown reloc.section");
            break;
        }
    }

    MemHandleUnlock(relocH);
    DmReleaseResource(relocH);
}

#if 0
/* Not used */
void
MultilinkRelocateData(UInt32 rsrcId, void* text_a)
{
    extern long data_start, bss_start;

    MultilinkRelocateDataRsrc(rsrcId, text_a, &data_start, &bss_start);
}
#endif

