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

ROOT_PATH = ../../..

UTILS_PATH = $(ROOT_PATH)/utils
UTILS_LIST = $(UTILS_PATH)/Exception.sml

SRC_LIST = ../src/Dictionary.sml

TEST_UNIT_PATH = $(ROOT_PATH)/testing
TEST_UNIT_LIST = $(TEST_UNIT_PATH)/src/Assert.sml $(TEST_UNIT_PATH)/src/Test.sml \
					$(TEST_UNIT_PATH)/src/TextUITestRunner.sml

TEST_LIST = *Tests.sml

.PHONY: all
all: Tests.cake

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

Tests.cake: Tests.cake.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

Tests.cake.S: Tests.cake.sml
	$(CAKEC) < $^ > Tests.cake.S

Tests.cake.sml: $(UTILS_LIST) $(SRC_LIST) $(TEST_UNIT_LIST) $(TEST_LIST) $(TEST_UNIT_PATH)/src/Exit.sml
	cat $^ > $@

clean: 
	$(RM) Tests.cake Tests.cake.S Tests.cake.sml basis_ffi.o

.INTERMEDIATE: Tests.cake.S Tests.cake.sml basis_ffi.o
