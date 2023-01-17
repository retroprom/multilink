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

# Parameters
VERSION ?= "0.3"
HOST ?= i686-linux-gnu
TARGET ?= m68k-palmos
PREFIX ?= /usr/local
PALMSDK ?= /opt/palmdev

# Tools
CC=gcc
AR=ar
HOST_CC ?= $(CC)
TARGET_CC ?= $(TARGET)-$(CC)
TARGET_AR ?= $(TARGET)-$(AR)

# Generic files
EXE = multilink
DOC = multilink.html
MAN = multilink.man

# Host files
HOST_EXE = $(TARGET)-$(EXE)
HOST_EXE_SRCS = multilink.c
HOST_EXE_OBJS = multilink.o
HOST_MAN= $(TARGET)-$(EXE).1
HOST_HDRS = \
	multilinkrloc.h
HOST_DOCS = \
	out/multilink.html \
	out/website.html

# Target files
TARGET_HDRS = \
	multilinkrt.h \
	multilinkrloc.h
TARGET_CRT_SRCS = \
	crt0.c \
	shlibcrt0.c
TARGET_CRT_OBJS = \
	out/$(TARGET)/crt/crt0.o \
	out/$(TARGET)/crt/gcrt0.o \
	out/$(TARGET)/crt/slcrt0.o \
	out/$(TARGET)/crt/slgcrt0.o
TARGET_LIB_SRCS = \
	multilinkget.c \
	multilinkload.c \
	multilinkrloc.c \
	multilinkdata.c \
	multilinkglobals.c
TARGET_LIB_OBJS = \
	out/$(TARGET)/lib/multilinkget.o \
	out/$(TARGET)/lib/multilinkload.o \
	out/$(TARGET)/lib/multilinkrloc.o \
	out/$(TARGET)/lib/multilinkdata.o \
	out/$(TARGET)/lib/multilinkglobals.o

# External paths
HOST_DIR=$(PREFIX)
HOST_BINDIR=$(HOST_DIR)/bin
HOST_DOCDIR=$(HOST_DIR)/share/doc
HOST_MANDIR=$(HOST_DIR)/share/man
BUILD_DIR=$(PREFIX)/$(HOST)/$(TARGET)
BUILD_LIBDIR=$(BUILD_DIR)/lib
BUILD_INCDIR=$(BUILD_DIR)/include
TARGET_DIR=$(PREFIX)/$(TARGET)
TARGET_LIBDIR=$(TARGET_DIR)/lib
TARGET_INCDIR=$(TARGET_DIR)/include

# Palm SDKs
PALMSDK2_INCDIR=$(PALMSDK)/sdk-2/include
PALMSDK2_INCS= \
		-nostdinc \
		-I$(PALMSDK2_INCDIR) \
		-I$(PALMSDK2_INCDIR)/Hardware \
		-I$(PALMSDK2_INCDIR)/System \
		-I$(PALMSDK2_INCDIR)/System/Unix \
		-I$(PALMSDK2_INCDIR)/UI
PALMSDK4_INCDIR=$(PALMSDK)/sdk-4/include
PALMSDK4_INCS= \
		-nostdinc \
		-I$(PALMSDK4_INCDIR) \
		-I$(PALMSDK4_INCDIR)/Core \
		-I$(PALMSDK4_INCDIR)/Core/Hardware \
		-I$(PALMSDK4_INCDIR)/Core/System \
		-I$(PALMSDK4_INCDIR)/Core/System/Unix \
		-I$(PALMSDK4_INCDIR)/Core/UI \
		-I$(PALMSDK4_INCDIR)/Libraries \
		-I$(PALMSDK4_INCDIR)/Libraries/PalmOSGlue \
		-I$(PALMSDK4_INCDIR)/Libraries/INet \
		-I$(PALMSDK4_INCDIR)/Libraries/Web \
		-I$(PALMSDK4_INCDIR)/Dynamic \
		-I$(PALMSDK4_INCDIR)/Locale

# Host details
HOST_DEFS=-DMULTILINK_NEW_HEADERS
HOST_INCS=-I$(BUILD_INCDIR)
HOST_LIBS=-L$(BUILD_LIBDIR) -lbfd -liberty
HOST_CFLAGS=-g -O2 -Wall -Wextra \
	$(HOST_INCS) $(HOST_DEFS) \
	-DMULTILINK_CC=\"$(TARGET_CC)\" \
	-DMULTILINK_LIBDIR=\"$(TARGET_LIBDIR)/multilink\" \
	-DMULTILINK_STDLIB=\"$(TARGET_STDLIB)\"

# Target details
TARGET_STDLIB=$(shell $(TARGET_CC) -print-search-dirs |sed -n 's/^libraries: //p')
TARGET_DEFS=
ifeq ($(PRC_TOOLS_VERSION),0_5)
  TARGET_DEFS+=-DMULTILINK_A4_GLOBALS -DMULTILINK_RELOCATION_OLD
  TARGET_INCS+=$(PALMSDK2_INCS)
else
  TARGET_DEFS+=-DMULTILINK_NEW_HEADERS
  TARGET_INCS+=$(PALMSDK4_INCS)
endif
TARGET_CFLAGS=-g -O2 -Wall $(TARGET_INCS) $(TARGET_DEFS)

# Default rule
all:	out/$(HOST)/$(HOST_EXE) out/$(HOST)/$(HOST_MAN) $(HOST_DOCS) $(TARGET_CRT_OBJS) $(TARGET_LIB_OBJS)

# Cleanup
clean:
	rm -rf out

# Install
install: all
	mkdir -p \
	  $(HOST_BINDIR) \
	  $(HOST_DOCDIR) \
	  $(HOST_MANDIR)/man1 \
	  $(BUILD_INCDIR) \
	  $(BUILD_LIBDIR) \
	  $(TARGET_INCDIR) \
	  $(TARGET_LIBDIR)/multilink/crt \
	  $(TARGET_LIBDIR)/multilink/lib
	install -m 755 out/$(HOST)/$(HOST_EXE) $(HOST_BINDIR)/$(HOST_EXE)
	install -m 644 out/$(HOST)/$(HOST_MAN) $(HOST_MANDIR)/man1/$(HOST_MAN)
	for o in $(HOST_DOCS) ; do \
	  install -m 644 $$o $(HOST_DOCDIR)/`basename $$o` ; \
	done
	for o in $(HOST_HDRS) ; do \
	  install -m 644 $$o $(HOST_INCDIR)/`basename $$o` ; \
	done
	for o in $(TARGET_CRT_OBJS) ; do \
	  install -m 644 $$o $(TARGET_LIBDIR)/multilink/crt/`basename $$o` ; \
	done
	for o in $(TARGET_LIB_OBJS) ; do \
	  install -m 644 $$o $(TARGET_LIBDIR)/multilink/lib/`basename $$o` ; \
	done
	for o in $(TARGET_HDRS) ; do \
	  install -m 644 $$o $(TARGET_INCDIR)/`basename $$o` ; \
	done

# Docs build
out/%.html: %.html
	cp $< $@
out/$(DOC): $(MAN)
	rman --filter HTML --title 'Multilink(1) Manual Page' $< > $@
out/$(HOST)/$(TARGET)-%.1: %.man
	cp $< $@

# Host build
out/$(HOST):
	[ -d $@ ] || mkdir -p $@
out/$(HOST)/$(HOST_EXE): $(HOST_EXE_SRCS) out/$(HOST)
	$(HOST_CC) $(HOST_CFLAGS) -o $@ $(HOST_EXE_SRCS) $(HOST_LIBS)

# Target build
out/$(TARGET):
	[ -d $@/crt ] || mkdir -p $@/crt
	[ -d $@/lib ] || mkdir -p $@/lib
out/$(TARGET)/lib/%.o: %.c out/$(TARGET)
	$(TARGET_CC) $(TARGET_CFLAGS) -c $< -o $@
out/$(TARGET)/crt/crt0.o: crt0.c out/$(TARGET)
	$(TARGET_CC) $(TARGET_CFLAGS) -c $< -o $@
out/$(TARGET)/crt/slcrt0.o: shlibcrt0.c out/$(TARGET)
	$(TARGET_CC) $(TARGET_CFLAGS) -c $< -o $@
out/$(TARGET)/crt/gcrt0.o: crt0.c out/$(TARGET)
	$(TARGET_CC) $(TARGET_CFLAGS) -DENABLE_START_DEBUG -c $< -o $@
out/$(TARGET)/crt/slgcrt0.o: shlibcrt0.c out/$(TARGET)
	$(TARGET_CC) $(TARGET_CFLAGS) -DENABLE_START_DEBUG -c $< -o $@

# Declare phony
.PHONY: all clean install
