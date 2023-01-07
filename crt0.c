/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 *  Pilot startup code for use with gcc.  This code was written 
 *  by Kresten Krab Thorup, and is in the public domain.
 *  It is *not* under the GPL or the GLPL, you can freely link it
 *  into your programs.
 *
 *  Hacks to support multilink world, djw@avantgo.com, July 15, 1998.
 */
#include "multilinkrt.h"

static void do_bhook(UInt16, void*, UInt16);
static void do_ehook(UInt16, void*, UInt16);
#ifdef ENABLE_START_DEBUG
static int  StartDebug(void);
#endif /*ENABLE_START_DEBUG*/

register UInt32 reg_a4 asm("%a4");
register UInt32 reg_a5 asm("%a5");

UInt32
start()
{
    SysAppInfoPtr appInfo;
    MemPtr        prevGlobals;
    MemPtr        globalsPtr;
    UInt32        result;
    Int16         mainCmd;
    void*         mainPBP;
    UInt16        mainFlags;
    unsigned**    jmptables;
#ifdef MULTILINK_A4_GLOBALS
    UInt32        save_a4 = reg_a4;
#else /* a5 */
    UInt32        save_a5 = reg_a5;
    MultilinkAppInfoAndJmpTable appInfoHook;
#endif

    if (SysAppStartup (&appInfo, &prevGlobals, &globalsPtr) != 0) {
        SndPlaySystemSound (sndError);
        return -1;
    }

    mainCmd = appInfo->cmd;
    mainPBP = appInfo->cmdPBP;
    mainFlags = appInfo->launchFlags;

    if (mainFlags & sysAppLaunchFlagNewGlobals) {
        extern long data_start, bss_start; 
#ifdef MULTILINK_A4_GLOBALS
        asm volatile ("move.l %a5,%a4; sub.l #edata,%a4");
#endif /*MULTILINK_A4_GLOBALS*/
        MultilinkRelocateDataRsrc(0, &start, &data_start, &bss_start);
#ifdef MULTILINK_A4_GLOBALS
    } else if ((mainFlags & sysAppLaunchFlagSubCall) != 0) {
        /* for a sub call, reget the a4 globals from a5 */
        asm volatile ("move.l %a5,%a4; sub.l #edata,%a4");
    } else {
        reg_a4 = 0;
#endif /*MULTILINK_A4_GLOBALS*/
    }

    /* load other segments */
    jmptables =
        MultilinkLoadCodeSegments(MULTILINK_APPL_ID,
                                  MULTILINK_SEGMENT_JMPTABLE,
                                  MULTILINK_NJMPTABLES,
                                  (mainFlags&sysAppLaunchFlagNewGlobals) != 0);
        
    if ((mainFlags & sysAppLaunchFlagNewGlobals) != 0) {
        MultilinkSegmentJmpTables = jmptables;
#ifndef MULTILINK_A4_GLOBALS
    } else if (!(mainFlags & sysAppLaunchFlagSubCall)) { /* keep sub globals */
        appInfoHook.appInfo = appInfo;
        appInfoHook.jmpTables = jmptables;
        reg_a5 = (UInt32)&appInfoHook.appInfo;
#endif
    }

    do_bhook(mainCmd, mainPBP, mainFlags);

#ifdef ENABLE_START_DEBUG
    StartDebug();
#endif /*ENABLE_START_DEBUG*/

    result = PilotMain(mainCmd, mainPBP, mainFlags);

    do_ehook(mainCmd, mainPBP, mainFlags);

    MultilinkUnloadCodeSegments(MULTILINK_APPL_ID,
                                jmptables,
                                MULTILINK_NJMPTABLES);
 
#ifdef MULTILINK_A4_GLOBALS
    reg_a4 = save_a4;
#else
    if (reg_a5 == (UInt32)&appInfoHook.appInfo) {
        reg_a5 = save_a5;
    }
#endif

    SysAppExit(appInfo, prevGlobals, globalsPtr);

    return result;
}

static void
do_bhook(UInt16 cmd, void* PBP, UInt16 flags)
{
    void **hookend, **hookptr;
    unsigned long text = (unsigned long)&start;

    asm ("sub.l #start, %0" : "=g" (text) : "0" (text));
    asm ("lea bhook_start(%%pc),%0" : "=a" (hookptr) :);
    asm ("lea bhook_end(%%pc),%0" : "=a" (hookend) :);

    while (hookptr < hookend) {
        void (*fptr)(UInt16, void*, UInt16) = (*(hookptr++)) + text;
        fptr(cmd,PBP,flags);
    }
}

static void
do_ehook(UInt16 cmd, void* PBP, UInt16 flags)
{
    void **hookstart, **hookptr;
    unsigned long text = (unsigned long)&start;
    asm ("sub.l #start, %0" : "=g" (text) : "0" (text));

    asm ("lea ehook_start(%%pc),%0" : "=a" (hookstart) :);
    asm ("lea ehook_end(%%pc),%0" : "=a" (hookptr) :);

    while (hookptr > hookstart) {
        void (*fptr)(UInt16, void*, UInt16) = (*(--hookptr)) + text;
        fptr(cmd,PBP,flags);
    }
}

#ifdef ENABLE_START_DEBUG
static int
StartDebug(void)
{
    UInt32 feature = 0;

#ifdef MULTILINK_A4_GLOBALS
    if (!reg_a4)
        return 0;
#endif

    FtrGet('gdbS', 0, &feature);
    if (feature != 0x12BEEF34)
        return 0;

#if 0
    /*causes POSE to hiccup now and is not needed*/
    /* get the ROM version.  PalmOS2 needs a flag set */
    FtrGet(sysFtrCreator, sysFtrNumROMVersion, &feature);
    if (feature >= 0x02000000) {
        (*(char*)257) = 1; /* Turn on debugging */
    }
#endif
  
#ifdef MULTILINK_A4_GLOBALS
    asm("
    lea data_start(%%a4),%%a1
    move.l %%a1,%%d2
    lea bss_start(%%a4),%%a1
    move.l %%a1,%%d1
    lea start(%%pc),%%a0
    move.l %%a0,%%d0
    sub.l #start, %%d0
    lea PilotMain(%%pc),%%a0
    move.l #0x12BEEF34, %%d3
    trap #8
    " : : : "d0", "d1", "d2", "d3", "a1", "a0");
#else
    asm("
    lea data_start@END(%%a5),%%a1
    move.l %%a1,%%d2
    lea bss_start@END(%%a5),%%a1
    move.l %%a1,%%d1
    lea start(%%pc),%%a0
    move.l %%a0,%%d0
    sub.l #start, %%d0
    lea PilotMain(%%pc),%%a0
    move.l #0x12BEEF34, %%d3
    trap #8
    " : : : "d0", "d1", "d2", "d3", "a1", "a0");
#endif
    return 1;
}
#endif /*ENABLE_START_DEBUG*/
