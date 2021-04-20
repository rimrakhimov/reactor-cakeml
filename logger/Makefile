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


SRC_LIST = src/LoggerLevel.sml src/Logger.sml src/LoggerInitializers.sml

.PHONY: all
all: Logger.cake

Logger.cake: Logger.cake.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

Logger.cake.S: Logger.cake.sml
	$(CAKEC) < $^ > Logger.cake.S

Logger.cake.sml: $(SRC_LIST)
	cat $^ > $@

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

clean: 
	$(RM) Logger.cake Logger.cake.S Logger.cake.sml basis_ffi.o

.INTERMEDIATE: Logger.cake.S Logger.cake.sml basis_ffi.o
