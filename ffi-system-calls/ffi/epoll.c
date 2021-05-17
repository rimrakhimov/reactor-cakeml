#include <assert.h>
#include <sys/epoll.h>

#include "include/codes.h"
#include "include/marshalling.h"

void ffiepoll_create(unsigned char *c, long clen, unsigned char *a, long alen)
{
    assert(5 <= alen);
    int fd = epoll_create1(0);
    if (0 <= fd)
    {
        a[0] = FFI_SUCCESS;
        int_to_byte4(fd, &a[1]);
    }
    else
        a[0] = FFI_FAILURE;
}