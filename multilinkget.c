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
 * multilinkget.c - runtime support for locating code segments.
 *
 * created: David Williams, djw@avantgo.com, July, 1998.
 */
#include "multilinkrt.h"

unsigned**
MultilinkGetCodeSegments(UInt32 creatorId)
{
    unsigned** jmptables;
	UInt32     featureId;
    Err        err;

	featureId = MULTILINK_JMPTABLES_FTR_ID(creatorId);
    err = FtrGet(creatorId, featureId, (UInt32 *)&jmptables);

    if (err != 0 || jmptables == NULL) {
        ErrFatalDisplayIf(1, "Could not find code segment jump tables");
        /*NOTREACHED*/
    }
    
    return jmptables;
}

