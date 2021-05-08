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

SRC_LIST = src/IOBuffer.sml

.PHONY: all
all: IOBuffer.cake

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

IOBuffer.cake: IOBuffer.cake.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

IOBuffer.cake.S: IOBuffer.cake.sml
	$(CAKEC) < $^ > IOBuffer.cake.S

IOBuffer.cake.sml: $(UTILS_LIST) $(SRC_LIST)
	cat $^ > $@

clean: 
	$(RM) IOBuffer.cake IOBuffer.cake.S IOBuffer.cake.sml basis_ffi.o

.INTERMEDIATE: IOBuffer.cake.S IOBuffer.cake.sml basis_ffi.o
