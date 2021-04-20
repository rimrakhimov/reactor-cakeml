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
UTILS_LIST = $(UTILS_PATH)/Exception.sml

SRC_LIST = src/Assert.sml src/Test.sml src/TextUITestRunner.sml src/Exit.sml

.PHONY: all
all: Testing.cake

Testing.cake: Testing.cake.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

Testing.cake.S: Testing.cake.sml
	$(CAKEC) < $^ > Testing.cake.S

Testing.cake.sml: $(UTILS_LIST) $(SRC_LIST)
	cat $^ > $@

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

clean: 
	$(RM) Testing.cake Testing.cake.S Testing.cake.sml basis_ffi.o

.INTERMEDIATE: Testing.cake.S Testing.cake.sml basis_ffi.o
