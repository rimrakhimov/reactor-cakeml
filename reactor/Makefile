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
UTILS_LIST = $(UTILS_PATH)/Exception.sml $(UTILS_PATH)/NetworkAddress.sml

BUFFER_PATH = $(ROOT_PATH)/buffer
BUFFER_LIST = $(BUFFER_PATH)/src/IOBuffer.sml

LOGGER_PATH = $(ROOT_PATH)/logger
LOGGER_LIST = $(LOGGER_PATH)/src/LoggerLevel.sml $(LOGGER_PATH)/src/Logger.sml

SRC_LIST = src/EventHandlers.sml src/FDInfo.sml src/Reactor.sml

.PHONY: all
all: Reactor.cake

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

Reactor.cake: Reactor.cake.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

Reactor.cake.S: Reactor.cake.sml
	$(CAKEC) < $^ > Reactor.cake.S

Reactor.cake.sml: $(UTILS_LIST) $(BUFFER_LIST) $(LOGGER_LIST) $(SRC_LIST)
	cat $^ > $@

clean: 
	$(RM) Reactor.cake Reactor.cake.S Reactor.cake.sml basis_ffi.o

.INTERMEDIATE: Reactor.cake.S Reactor.cake.sml basis_ffi.o
