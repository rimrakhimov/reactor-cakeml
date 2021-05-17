#include <assert.h>
#include <unistd.h>

#include "include/codes.h"
#include "include/marshalling.h"

void ffifd_close(unsigned char *c, long clen, unsigned char *a, long alen)
{
    assert(clen == 4);
    assert(alen == 1);

    int fd = byte4_to_int(c);
    if (close(fd) == 0)
        a[0] = FFI_SUCCESS;
    else
        a[0] = FFI_FAILURE;
}