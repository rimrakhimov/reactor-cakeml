#include <assert.h>
#include <errno.h>
#include <sys/epoll.h>

#include "include/codes.h"
#include "include/epoll_codes.h"
#include "include/marshalling.h"

uint32_t ffi_events_to_events(int c_events) {
    uint32_t events = 0;
    if ((c_events & FFI_EPOLL_EPOLLIN) > 0) 
        events |= EPOLLIN;
    if ((c_events & FFI_EPOLL_EPOLLOUT) > 0)
        events |= EPOLLOUT;
    if ((c_events & FFI_EPOLL_EPOLLRDHUP) > 0)
        events |= EPOLLRDHUP;
    if ((c_events & FFI_EPOLL_EPOLLPRI) > 0)
        events |= EPOLLPRI;
    if ((c_events & FFI_EPOLL_EPOLLERR) > 0)
        events |= EPOLLERR;
    if ((c_events & FFI_EPOLL_EPOLLHUP) > 0)
        events |= EPOLLHUP;
    if ((c_events & FFI_EPOLL_EPOLLET) > 0)
        events |= EPOLLET;
    if ((c_events & FFI_EPOLL_EPOLLONESHOT) > 0)
        events |= EPOLLONESHOT;
    
    return events;
}

uint32_t events_to_ffi_events(int events) {
    uint32_t c_events = 0;
    if ((events & EPOLLIN) > 0)
        c_events |= FFI_EPOLL_EPOLLIN;
    if ((events & EPOLLOUT) > 0)
        c_events |= FFI_EPOLL_EPOLLOUT;
    if ((events & EPOLLRDHUP) > 0)
        c_events |= FFI_EPOLL_EPOLLRDHUP;
    if ((events & EPOLLPRI) > 0)
        c_events |= FFI_EPOLL_EPOLLPRI;
    if ((events & EPOLLERR) > 0)
        c_events |= FFI_EPOLL_EPOLLERR;
    if ((events & EPOLLHUP) > 0)
        c_events |= FFI_EPOLL_EPOLLHUP;
    if ((events & EPOLLET) > 0)
        c_events |= FFI_EPOLL_EPOLLET;
    if ((events & EPOLLONESHOT) > 0)
        c_events |= FFI_EPOLL_EPOLLONESHOT;

    return c_events;
}

int ffi_op_code_to_op_code(int c_op_code) {
    switch (c_op_code) {
        case FFI_EPOLL_CTL_ADD:
            return EPOLL_CTL_ADD;
        case FFI_EPOLL_CTL_MOD:
            return EPOLL_CTL_MOD;
        case FFI_EPOLL_CTL_DEL:
            return EPOLL_CTL_DEL;
        default:
            /* Ensures that the opcode is invalid such that EINVAL
       * error will be returned back. */
            return EPOLL_CTL_ADD | EPOLL_CTL_MOD | EPOLL_CTL_DEL;
    }
}

// Epoll file descriptor creation.
// INPUT:  0 bytes
// OUTPUT: 1 byte - result code, 4 bytes - file descriptor
void ffiepoll_create(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(alen == 5);
    int fd = epoll_create1(0);
    if (0 <= fd) {
        a[0] = FFI_SUCCESS;
        int_to_byte4(fd, &a[1]);
    } else
        a[0] = FFI_FAILURE;
}

// Epoll Control system call
// INPUT:  1 byte - command, 4 bytes - epollfd, 4 bytes - fd, 1 bytes - events
// OUTPUT: 1 byte - code result
void ffiepoll_ctl(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 10);
    assert(alen == 1);

    int c_op_code = c[0];
    int c_epoll_fd = byte4_to_int(&c[1]);
    int c_fd = byte4_to_int(&c[5]);
    uint8_t c_events = c[9];

    struct epoll_event ev;
    ev.data.fd = c_fd;
    ev.events = ffi_events_to_events(c_events);

    int op_code = ffi_op_code_to_op_code(c_op_code);
    int status = epoll_ctl(c_epoll_fd, op_code, ev.data.fd, &ev);
    if (status == 0)
        a[0] = FFI_SUCCESS;
    else
        a[0] = FFI_FAILURE;
}

// Epoll Wait system call
// INPUT:  4 bytes - epollfd, 4 bytes - number of events, 8 bytes - timeout in
// milliseconds. OUTPUT: 1 byte - code result, 4 bytes - n: number of ready file
// descriptors,
//      next are n events, each has 5 bytes: 4 bytes - fd, 1 byte - events
void ffiepoll_wait(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 16);

    int c_epoll_fd = byte4_to_int(c);
    int c_max_events = byte4_to_int(&c[4]);
    int c_timeout_msec = byte4_to_int(&c[8]);

    assert(alen == 1 + 4 + c_max_events * 5);

    struct epoll_event ev[c_max_events];
    int n = epoll_wait(c_epoll_fd, ev, c_max_events, c_timeout_msec);
    if (n > 0) {
        a[0] = FFI_SUCCESS;
        int_to_byte4(n, &a[1]);
        for (int i = 0; i < n; i++) {
            int fd = ev[i].data.fd;
            int events = ev[i].events;
            int_to_byte4(fd, &a[5 + i * 5]);
            a[5 + i * 5 + 4] = events_to_ffi_events(events);
        }
    } else {
        if (errno == EINTR) {
            a[0] = FFI_EINTR;
        } else {
            a[0] = FFI_FAILURE;
        }
    }
}