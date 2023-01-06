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
#ifndef __MULTILINKRLOC_H__
#define __MULTILINKRLOC_H__

typedef struct pilot_reloc {
    unsigned char  type;
    unsigned char  section;  
    unsigned short offset;
    long           value ;
} pilot_reloc;

#define TEXT_SECTION 't'
#define DATA_SECTION 'd'
#define BSS_SECTION  'b'

#define RELOC_ABS_32 0xbe

#endif /*__MULTILINKRLOC_H__*/
