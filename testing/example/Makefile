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

ROOT_PATH = ../..

UTILS_PATH = $(ROOT_PATH)/utils
UTILS_LIST = $(UTILS_PATH)/Exception.sml

SRC_LIST = src/Dictionary.sml

.PHONY: all
all: Dictionary.cake

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

Dictionary.cake: Dictionary.cake.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

Dictionary.cake.S: Dictionary.cake.sml
	$(CAKEC) < $^ > Dictionary.cake.S

Dictionary.cake.sml: $(UTILS_LIST) $(SRC_LIST)
	cat $^ > $@

clean: 
	$(RM) Dictionary.cake Dictionary.cake.S Dictionary.cake.sml basis_ffi.o

.INTERMEDIATE: Dictionary.cake.S Dictionary.cake.sml basis_ffi.o
