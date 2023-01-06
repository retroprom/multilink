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
 * multilinkdata.c - runtime support for loading data segments.
 *
 * created: David Williams, djw@avantgo.com, March, 1999.
 */
#include "multilinkrt.h"
#include "multilinkrloc.h"

typedef union GetData {
	char	raw[4];
	short	word;
	long	lword;
}	GetData; 

static char * PrvDecompressData(char *ptr,char *datasegment)
{
	GetData		ldata;
	int			i,data;
	char		*to,c;
	

	for(i=0; i<3; i++)
	{
		ldata.raw[0]=*ptr++; ldata.raw[1]=*ptr++; ldata.raw[2]=*ptr++; ldata.raw[3]=*ptr++;
		to=datasegment+ldata.lword;
		while(1)
		{
			data=*ptr++;

			//	decompress (x&0x7f)+1 raw data bytes
			if(data&0x80) {	
				data&=0x7F; 
				do *to++=*ptr++;
					while(--data>=0); 
				continue;
				}
				
			//	decompress (x&0x3f)+1 0x00 data bytes
			//	data is already initilized to 0x00
			if(data&0x40) {	
				to+=(data&0x3F)+1; 
				continue;	
				}
				
			//	decompress (x&0x1f)+2 repeating data bytes
			if(data&0x20) {	
				data=(data&0x1F)+1; 
				c=*ptr++; 
				goto cloop;
				}
				
			//	decompress (x&0x0f)+1 0xFF data bytes
			if(data&0x10) {	
				data&=0x0F; c=0xFF;
cloop:			
				do *to++=c; 
					while(--data>=0); 
				continue;
				}
				
			switch(data) {
				case 0x00: break;
				case 0x01: to+=4; *to++=0xFF; *to++=0xFF; *to++=*ptr++; *to++=*ptr++; continue;
				case 0x02: to+=4; *to++=0xFF; *to++=*ptr++; *to++=*ptr++; *to++=*ptr++; continue;
				case 0x03: *to++=0xA9; *to++=0xF0; to+=2; *to++=*ptr++; *to++=*ptr++; to++; *to++=*ptr++; continue;
				case 0x04: *to++=0xA9; *to++=0xF0; to++; *to++=*ptr++; *to++=*ptr++; *to++=*ptr++; to++; *to++=*ptr++; continue;
				default:   DbgBreak(); break;
				}
			break;
			} // While (1)
			
		} // for (i=0; i<3; i++)

	return ptr;
}

/*
 *  This function should be called from SysLibs to load their own data.
 */
void*
MultilinkLoadData()
{
    MemHandle codeH;
    MemHandle dataH;
    unsigned char* globals = NULL;
    UInt32    above;
    UInt32    below;

    /* Do we have a code0000 resource? */
    codeH = DmGet1Resource('code', 0);
    if (codeH != 0) {
        UInt32* codeP;

        /*
         * code0000[long 0] nBytesAboveA5
         * code0000[long 1] nBytesBelowA5
         */
        codeP = (UInt32*)MemHandleLock(codeH);
        above = codeP[0];
        below = codeP[1];
			
        MemPtrUnlock(codeP);
        DmReleaseResource(codeH);

        if (above < 4)
            above = 4; /* for *AppInfo */
    } else { /* no code0000 */
        above = 4; /* sizeof(AppInfo*) */
        below = 0;
    }

    dataH = DmGet1Resource('data', 0);
    if (dataH != 0) {
        char* dataP = MemHandleLock(dataH);

        if (dataP != NULL) {

            UInt32 total_len = above + below;

            globals = MemPtrNew(total_len);
        
            if (globals != NULL) {
                char* a5p;

                MemSet(globals, total_len, 0);

                a5p = globals + below;

                dataP = PrvDecompressData(dataP+4, (char*)a5p);

                MemPtrSetOwner(globals, 0);
            }
        }

        MemHandleUnlock(dataH);
        DmReleaseResource(dataH);
    }

    return globals;
}
