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
UTILS_LIST = $(UTILS_PATH)/Exception.sml $(UTILS_PATH)/Result.sml $(UTILS_PATH)/ByteArray.sml $(UTILS_PATH)/NetworkAddress.sml

BUFFER_PATH = $(ROOT_PATH)/buffer
BUFFER_LIST = $(BUFFER_PATH)/src/IOBuffer.sml

LOGGER_PATH = $(ROOT_PATH)/logger
LOGGER_LIST = $(LOGGER_PATH)/src/LoggerLevel.sml $(LOGGER_PATH)/src/Logger.sml \
				$(LOGGER_PATH)/src/LoggerInitializers.sml

FFI_SYSTEM_CALLS_PATH = $(ROOT_PATH)/ffi-system-calls
FFI_SYSTEM_CALLS_LIST = $(FFI_SYSTEM_CALLS_PATH)/MarshallingHelp.sml \
							$(FFI_SYSTEM_CALLS_PATH)/FFICommon.sml \
							$(FFI_SYSTEM_CALLS_PATH)/Errno.sml \
							$(FFI_SYSTEM_CALLS_PATH)/Fd.sml \
							$(FFI_SYSTEM_CALLS_PATH)/epoll/EpollOpCode.sml \
							$(FFI_SYSTEM_CALLS_PATH)/epoll/EpollEvent.sml \
							$(FFI_SYSTEM_CALLS_PATH)/epoll/Epoll.sml \
							$(FFI_SYSTEM_CALLS_PATH)/Socket.sml \
							$(FFI_SYSTEM_CALLS_PATH)/Timer.sml \
							$(FFI_SYSTEM_CALLS_PATH)/Timestamp.sml

TIME_PATH = $(ROOT_PATH)/time
TIME_LIST = $(TIME_PATH)/src/Time.sml

SRC_LIST = ../src/IO.sml ../src/EventHandlers.sml ../src/FDInfo.sml ../src/Reactor.sml

TEST_UNIT_PATH = $(ROOT_PATH)/testing
TEST_UNIT_LIST = $(TEST_UNIT_PATH)/src/Assert.sml $(TEST_UNIT_PATH)/src/Test.sml \
					$(TEST_UNIT_PATH)/src/TextUITestRunner.sml

<<<<<<< HEAD
TEST_LIST = src/ReactorTimerTests.sml # src/reactor-file-tests/ReactorFileTests.sml
=======
TEST_LIST = src/reactor-acceptor-tests/ReactorAcceptorTests.sml \
			src/reactor-file-tests/ReactorFileTests.sml src/*Tests.sml
>>>>>>> functional-style

.PHONY: all
all: Tests.cake

<<<<<<< HEAD
=======
reactor_acceptor_tests.o: src/reactor-acceptor-tests/ffi/reactor_acceptor_tests.c
	$(CC) $(CFLAGS) -o $@ -c $^

>>>>>>> functional-style
reactor_file_tests_ffi.o: src/reactor-file-tests/ffi/reactor_file_tests_ffi.c
	$(CC) $(CFLAGS) -o $@ -c $^

timestamp_ffi.o: $(FFI_SYSTEM_CALLS_PATH)/ffi/timestamp.c
	$(CC) $(CFLAGS) -o $@ -c $^

timer_ffi.o: $(FFI_SYSTEM_CALLS_PATH)/ffi/timer.c
	$(CC) $(CFLAGS) -o $@ -c $^

socket_ffi.o: $(FFI_SYSTEM_CALLS_PATH)/ffi/socket.c
	$(CC) $(CFLAGS) -o $@ -c $^

epoll_ffi.o: $(FFI_SYSTEM_CALLS_PATH)/ffi/epoll.c
	$(CC) $(CFLAGS) -o $@ -c $^

fd_ffi.o: $(FFI_SYSTEM_CALLS_PATH)/ffi/fd.c
	$(CC) $(CFLAGS) -o $@ -c $^

errno_ffi.o: $(FFI_SYSTEM_CALLS_PATH)/ffi/errno.c
	$(CC) $(CFLAGS) -o $@ -c $^

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

Tests.cake: Tests.cake.S basis_ffi.o \
<<<<<<< HEAD
		errno_ffi.o fd_ffi.o epoll_ffi.o timer_ffi.o \
		timestamp_ffi.o reactor_file_tests_ffi.o
=======
		errno_ffi.o fd_ffi.o epoll_ffi.o socket_ffi.o \
		timer_ffi.o timestamp_ffi.o \
		reactor_file_tests_ffi.o reactor_acceptor_tests.o
>>>>>>> functional-style
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

Tests.cake.S: Tests.cake.sml
	$(CAKEC) < $^ > Tests.cake.S

Tests.cake.sml: $(UTILS_LIST) $(BUFFER_LIST) $(LOGGER_LIST) $(FFI_SYSTEM_CALLS_LIST) $(SRC_LIST) \
		 $(TIME_LIST) $(TEST_UNIT_LIST) $(TEST_LIST)
	cat $^ > $@

clean: 
	$(RM) Tests.cake Tests.cake.S Tests.cake.sml basis_ffi.o \
<<<<<<< HEAD
		errno_ffi.o fd_ffi.o epoll_ffi.o timer_ffi.o \
		timestamp_ffi.o reactor_file_tests_ffi.o

.INTERMEDIATE: Tests.cake.S Tests.cake.sml basis_ffi.o \
		errno_ffi.o fd_ffi.o epoll_ffi.o timer_ffi.o \
		timestamp_ffi.o reactor_file_tests_ffi.o
=======
		errno_ffi.o fd_ffi.o epoll_ffi.o socket_ffi.o \
		timer_ffi.o timestamp_ffi.o \
		reactor_file_tests_ffi.o reactor_acceptor_tests.o

.INTERMEDIATE: Tests.cake.S Tests.cake.sml basis_ffi.o \
		errno_ffi.o fd_ffi.o epoll_ffi.o socket_ffi.o \
		timer_ffi.o timestamp_ffi.o \
		reactor_file_tests_ffi.o reactor_acceptor_tests.o
>>>>>>> functional-style
