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
 * The Initial Developer of the Original Code is David Williams.
 * Portions created by David Williams are Copyright (C) 2001 David Williams.
 * All Rights Reserved.
 *
 * multilinkglobals.c - holder of the MultilinkSegmentJmpTables symbol.
 *
 * created: David Williams, djw@avantgo.com, July, 2001.
 */
#include "multilinkrt.h"

unsigned** MultilinkSegmentJmpTables; /* common: jmptable pointers  */
