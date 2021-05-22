#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "include/codes.h"
#include "include/marshalling.h"

void ffifd_close(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 4);
    assert(alen == 1);

    int fd = byte4_to_int(c);
    if (close(fd) == 0)
        a[0] = FFI_SUCCESS;
    else
        a[0] = FFI_FAILURE;
}

void ffifd_read(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 8);

    int c_fd = byte4_to_int(c);
    int c_count = byte4_to_int(&c[4]);

    assert(alen == 5 + c_count);

    int n = read(c_fd, &a[5], c_count);
    if (n >= 0) {
        a[0] = FFI_SUCCESS;
        int_to_byte4(n, &a[1]);
    } else {
        if (errno == EWOULDBLOCK || errno == EAGAIN) {
            a[0] = FFI_EAGAIN;
        } else if (errno == EINTR) {
            a[0] = FFI_EINTR;
        } else {
            a[0] = FFI_FAILURE;
        }
    }
}

void ffifd_set_blocking(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 5);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_blocking = c[4];

    int flags = fcntl(c_fd, F_GETFL, 0);
    if (flags < 0) {
        a[0] = FFI_FAILURE;
        return;
    }

    int was_blocking = !(flags & O_NONBLOCK);
    // Actually set or clear the Blocking mode,
    // if what we got is not what we want:
    if (was_blocking && !c_blocking || !was_blocking && c_blocking) {
        int rc = fcntl(c_fd, F_SETFL, flags ^ O_NONBLOCK);
        if (rc < 0) {
            a[0] = FFI_FAILURE;
            return;
        }
    }
    a[0] = FFI_SUCCESS;
}