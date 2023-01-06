#!make
#
# The contents of this file are subject to the Mozilla Public License
# Version 1.0 (the "License"); you may not use this file except in
# compliance with the License. You may obtain a copy of the License at
# http://www.mozilla.org/MPL/
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
# License for the specific language governing rights and limitations
# under the License.
#
# The Original Code is Multilink.
#
# The Initial Developer of the Original Code is AvantGo, Inc.
# Portions created by AvantGo, Inc. are Copyright (C) 1998-1999
# AvantGo, Inc. All Rights Reserved.
#
# created: David Williams, djw@avantgo.com, July, 1998.
#

# Specify target prc-tools set.
# values are 0_5 or 2_0
PRC_TOOLS_VERSION ?= 2_0
#PRC_TOOLS_VERSION ?= 0_5

ifeq ($(PRC_TOOLS_VERSION),0_5)
  PALM_TOOLS_PREFIX=m68k-palmos-coff-
  PRC_TOOLS_DEFINES=-DA4_GLOBALS -DRELOCATION_OLD
else
  PALM_TOOLS_PREFIX=m68k-palmos-
endif

# Location of standard Pilot distribution
PILOT_DIR = /usr/local/pilot
INSTALL_LIBDIR=$(PILOT_DIR)/lib
INSTALL_BINDIR=/usr/local/bin
INSTALL_MANDIR=/usr/local/man/man1
TARGET_LIBFILE_PREFIX=$(PALM_TOOLS_PREFIX)multilink
EXE_NAME=$(PALM_TOOLS_PREFIX)multilink
# Old names:
#TARGET_LIBFILE_PREFIX=multilink
#EXE_NAME=multilink

# For multilink program
INCLUDES  = -I${PILOT_DIR}/include
LIBS      = -L${PILOT_DIR}/lib -lbfd -liberty
CC        = gcc
DEFAULT_USE_COMPILER = $(PALM_TOOLS_PREFIX)gcc
DEFAULT_STDLIB_PATH = \
$(shell $(DEFAULT_USE_COMPILER) -print-search-dirs |sed -n 's/^libraries: //p')
CFLAGS    = -g $(INCLUDES) -Wall -O2 \
	-DTARGET_LIBDIR=\"$(INSTALL_LIBDIR)\" \
	-DTARGET_LIBFILE_PREFIX=\"$(TARGET_LIBFILE_PREFIX)\" \
	-DDEFAULT_USE_COMPILER=\"$(DEFAULT_USE_COMPILER)\" \
	-DDEFAULT_STDLIB_PATH=\"$(DEFAULT_STDLIB_PATH)\" \
	$(PRC_TOOLS_DEFINES) \
#
LD        = gcc
LDFLAGS   = -g

#
# For runtime libraries.
#
# Flags here are used to build the runtime libraries which will run
# on the PalmOS device.
# At the moment, the runtime library needs to be built against the
# the older header files. 3.1 or older should be OK.
PALMOS2_INCLUDES_DIR=$(PILOT_DIR)/m68k-palmos-coff/include/PalmOS2
PALMOS2_INCLUDES= \
		-nostdinc \
		-I$(PALMOS2_INCLUDES_DIR) \
		-I$(PALMOS2_INCLUDES_DIR)/Hardware \
		-I$(PALMOS2_INCLUDES_DIR)/System \
		-I$(PALMOS2_INCLUDES_DIR)/System/Unix \
		-I$(PALMOS2_INCLUDES_DIR)/UI \
#

PALMOS40_INCLUDES_DIR=$(PILOT_DIR)/m68k-palmos-coff/include/PalmOS40
PALMOS40_INCLUDES= \
		-nostdinc \
		-I$(PALMOS40_INCLUDES_DIR) \
		-I$(PALMOS40_INCLUDES_DIR)/Core \
		-I$(PALMOS40_INCLUDES_DIR)/Core/Hardware \
		-I$(PALMOS40_INCLUDES_DIR)/Core/System \
		-I$(PALMOS40_INCLUDES_DIR)/Core/System/Unix \
		-I$(PALMOS40_INCLUDES_DIR)/Core/UI \
		-I$(PALMOS40_INCLUDES_DIR)/Libraries \
		-I$(PALMOS40_INCLUDES_DIR)/Libraries/PalmOSGlue \
		-I$(PALMOS40_INCLUDES_DIR)/Libraries/INet \
		-I$(PALMOS40_INCLUDES_DIR)/Libraries/Web \
		-I$(PALMOS40_INCLUDES_DIR)/Dynamic \
		-I$(PALMOS40_INCLUDES_DIR)/Locale \
#

# For runtime libraries.

ifeq ($(PRC_TOOLS_VERSION),0_5)
  TARGET_CFLAGS = -O2 -g $(PALMOS2_INCLUDES) -DMULTILINK_GLOBALS_A4 -Wall
else
  TARGET_CFLAGS = -O2 -g -DMULTILINK_NEW_HEADERS $(PALMOS40_INCLUDES)
endif

TARGET_CC = $(PALM_TOOLS_PREFIX)gcc
TARGET_AR = $(PALM_TOOLS_PREFIX)ar

# For tarball rule
TARDIR    = multilink

TEST_DIR = tests

# Host objects
OBJS = multilink.o

# Pilot objects
TARGET_OBJS = \
	$(TARGET_LIBFILE_PREFIX)get.o \
	$(TARGET_LIBFILE_PREFIX)load.o \
	$(TARGET_LIBFILE_PREFIX)rloc.o \
	$(TARGET_LIBFILE_PREFIX)crt0.o \
	$(TARGET_LIBFILE_PREFIX)gcrt0.o \
	$(TARGET_LIBFILE_PREFIX)slcrt0.o \
	$(TARGET_LIBFILE_PREFIX)slgcrt0.o \
	$(TARGET_LIBFILE_PREFIX)data.o \
	$(TARGET_LIBFILE_PREFIX)globals.o \
#

SRCS = multilink.c
TARGET_SRCS = \
	multilinkget.c \
	multilinkload.c \
	multilinkrloc.c \
	multilinkdata.c \
	crt0.c \
	shlibcrt0.c \
	multilinkglobals.c \
#

TARGET_HSRCS = \
	multilinkrt.h \
	multilinkrloc.h \
#

DOCBITS = multilink.man multilink.html

TARBITS = Makefile $(SRCS) $(TARGET_SRCS) $(TARGET_HSRCS) $(DOCBITS)

all:		$(EXE_NAME) $(TARGET_OBJS)

$(EXE_NAME):	$(OBJS)
		$(LD) $(LDFLAGS) -o $(EXE_NAME) $(OBJS) $(LIBS)

$(TARGET_LIBFILE_PREFIX)get.o: multilinkget.c
	$(TARGET_CC) $(TARGET_CFLAGS) -c multilinkget.c -o $@

$(TARGET_LIBFILE_PREFIX)load.o: multilinkload.c
	$(TARGET_CC) $(TARGET_CFLAGS) -c $< -o $@

$(TARGET_LIBFILE_PREFIX)rloc.o:	multilinkrloc.c
	$(TARGET_CC) $(TARGET_CFLAGS) -c $< -o $@

$(TARGET_LIBFILE_PREFIX)data.o:	multilinkdata.c
	$(TARGET_CC) $(TARGET_CFLAGS) -c $< -o $@

$(TARGET_LIBFILE_PREFIX)globals.o:	multilinkglobals.c
	$(TARGET_CC) $(TARGET_CFLAGS) -c $< -o $@

$(TARGET_LIBFILE_PREFIX)crt0.o:	crt0.c
	$(TARGET_CC) $(TARGET_CFLAGS) -c crt0.c -o $@

$(TARGET_LIBFILE_PREFIX)gcrt0.o: crt0.c
	$(TARGET_CC) $(TARGET_CFLAGS) -DENABLE_START_DEBUG -c crt0.c -o $@

$(TARGET_LIBFILE_PREFIX)slcrt0.o: shlibcrt0.c
	$(TARGET_CC) $(TARGET_CFLAGS) -c shlibcrt0.c -o $@

$(TARGET_LIBFILE_PREFIX)slgcrt0.o: shlibcrt0.c
	$(TARGET_CC) $(TARGET_CFLAGS) -DENABLE_START_DEBUG -c shlibcrt0.c -o $@

tests:	$(EXE_NAME)  $(TARGET_OBJS)
	(cd $(TEST_DIR)/glib; $(TARGET_AR) x libglib.a)
	for dir in $(TEST_DIR)/?; do \
	    echo $$dir; (cd $$dir; $(TARGET_AR) x libobjs.a) ; \
	    ./$(EXE_NAME) -libdir . -use-compiler $(PALM_TOOLS_PREFIX)gcc \
		-segmentsize 28k \
		$$dir/*.o $(TEST_DIR)/glib/*.o ; \
	done

# Must be root...
install: all
	@test `whoami` = root || (echo "Must be root to install"; false)
	mkdir -p $(INSTALL_BINDIR)
	mkdir -p $(INSTALL_LIBDIR)
	cp $(EXE_NAME) $(INSTALL_BINDIR)/$(EXE_NAME)
	chmod 755 $(INSTALL_BINDIR)/$(EXE_NAME)
	for o in $(TARGET_OBJS) ; do \
	    cp $$o $(INSTALL_LIBDIR)/$$o ; \
	    chmod 664 $(INSTALL_LIBDIR)/$$o ; \
	done
	mkdir -p $(INSTALL_MANDIR)
	cp multilink.man $(INSTALL_MANDIR)/multilink.1
	chmod 644 $(INSTALL_MANDIR)/multilink.1

release: all
	mkdir -p Release/lib
	cp $(EXE_NAME) Release
	chmod 755 Release/$(EXE_NAME)
	cp $(TARGET_OBJS) Release/lib
	chmod 664 Release/lib/*.o

multilink.tgz: $(TARBITS)
	rm -rf $(TARDIR)
	mkdir -p $(TARDIR)
	cp $(TARBITS) $(TARDIR)
	chmod 664 $(TARDIR)/*
	tar cf - $(TARDIR) | gzip > multilink.tgz
	rm -rf $(TARDIR)

tarball: multilink.tgz

BINBITS = $(TARGET_OBJS) Makefile $(EXE_NAME) $(DOCBITS)
multilink-bin.tgz: $(BINBITS)
	mv $(EXE_NAME) multilink.exe
	rm -rf $(TARDIR)
	mkdir -p $(TARDIR)
	cp $(TARGET_OBJS) Makefile $(DOCBITS) $(TARDIR)
	cp multilink.exe multilink/$(EXE_NAME)
	chmod 664 $(TARDIR)/*
	tar cf - $(TARDIR) | gzip > multilink-bin.tgz
	rm -rf $(TARDIR)

binball: multilink-bin.tgz

clean:
	rm -f *.o *~ $(EXE_NAME) *.grc o???? \
		$(TEST_DIR)/?/*.o $(TEST_DIR)/glib/*.o multilink.html

%-asm.s: %.c
	$(TARGET_CC) -c -g -Wa,-a $(TARGET_CFLAGS) $< > $@

multilink.html: multilink.man
	rman --filter HTML --title 'Multilink(1) Manual Page' $< > $@

