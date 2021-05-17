#include <assert.h>
#include <errno.h>
#include <string.h>

#include "include/marshalling.h"

void ffierrno_errno(unsigned char *c, long clen, unsigned char *a, long alen)
{
    assert(4 <= alen);
    int_to_byte4(errno, a);
}

void ffierrno_strerror(unsigned char *c, long clen, unsigned char *a, long alen)
{
    assert(4 <= clen);
    int _errno = byte4_to_int(c);

    /*  The GNU C Library uses a buffer of 1024 characters for
     *  strerror(). Thus, we require at least 2 bytes for the length,
     *  and 1024 bytes for the result.  
     */
    assert(1026 <= alen);
    char *msg = strerror(_errno);
    size_t msg_len = strlen(msg);
    a[0] = msg_len;
    strncpy(&a[1], msg, msg_len);
}