/*
 *   Implements the foreign function interface (FFI) used in the Time structure,
 *   as a thin wrapper around the relevant system calls.
 */

#include <assert.h>
#include <stdint.h>
#include <time.h>

#include "include/codes.h"
#include "include/marshalling.h"

void ffiget_timestamp(unsigned char *c, long clen, unsigned char *a, long alen) {
    // First byte for error status, and 8 bytes for timestamp
    assert(alen == 9);

    struct timespec tms;
    int status = clock_gettime(CLOCK_REALTIME, &tms);

    if (status == 0) {
        a[0] = FFI_SUCCESS;
        uint64_t timestamp_usec = (uint64_t)(tms.tv_sec) * (uint64_t)1000000000 + (uint64_t)(tms.tv_nsec);
        int64_to_byte8(timestamp_usec, &a[1]);
    } else {
        a[0] = FFI_FAILURE;
    }
}
