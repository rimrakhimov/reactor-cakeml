#include <assert.h>
#include <errno.h>
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

    char *buf = malloc(sizeof(char) * c_count);
    if (c_count > 0 && buf == NULL) {
        a[0] = FFI_FAILURE;
        return;
    }

    int n = read(c_fd, buf, c_count);
    if (n >= 0) {
        a[0] = FFI_SUCCESS;
        int_to_byte4(n, &a[1]);
        memcpy(&a[5], buf, n);
    } else {
        if (errno == EWOULDBLOCK || errno == EAGAIN) {
            a[0] = FFI_EAGAIN;
        } else if (errno == EINTR) {
            a[0] = FFI_EINTR;
        } else {
            a[0] = FFI_FAILURE;
        }
    }

    free(buf);
}