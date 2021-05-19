# Change this directory if necessary  -- or
# provide the directory for your machine on the make command-line, e.g.
# make -n   CAKE_DIR="/someOtherLocation/cake-x64-64"
CAKE_DIR = ~/cake-x64-64
CAKEC = $(CAKE_DIR)/cake
BASIS = $(CAKE_DIR)/basis_ffi.c

OS ?= $(shell uname)
ifeq ($(OS),Darwin)
	# These options avoid linker warnings on macOS
	LDFLAGS += -Wl,-no_pie
endif

DEBUG = true
ifeq ($(DEBUG), true)
	CFLAGS += -ggdb3
else
	CFLAGS = -DNDEBUG
endif

CC = gcc
# CFLAGS =

ROOT_PATH = ..

UTILS_PATH = $(ROOT_PATH)/utils
UTILS_LIST = $(UTILS_PATH)/Exception.sml $(UTILS_PATH)/ByteArray.sml

FFI_SYSTEM_CALLS_PATH = $(ROOT_PATH)/ffi-system-calls
FFI_SYSTEM_CALLS_LIST = $(FFI_SYSTEM_CALLS_PATH)/MarshallingHelp.sml \
							$(FFI_SYSTEM_CALLS_PATH)/FFICommon.sml \
							$(FFI_SYSTEM_CALLS_PATH)/Timestamp.sml

SRC_LIST = src/Time.sml

.PHONY: all
all: Time.cake

timestamp_ffi.o: $(FFI_SYSTEM_CALLS_PATH)/ffi/timestamp.c
	$(CC) $(CFLAGS) -o $@ -c $^

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

Time.cake: Time.cake.S basis_ffi.o timestamp_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

Time.cake.S: Time.cake.sml
	$(CAKEC) < $^ > Time.cake.S

Time.cake.sml: $(UTILS_LIST) $(FFI_SYSTEM_CALLS_LIST) $(SRC_LIST)
	cat $^ > $@

clean: 
	$(RM) Time.cake Time.cake.S Time.cake.sml basis_ffi.o timestamp_ffi.o

.INTERMEDIATE: Time.cake.S Time.cake.sml basis_ffi.o timestamp_ffi.o
