#include <assert.h>
#include <sys/timerfd.h>

#include "include/codes.h"
#include "include/marshalling.h"

// Timer file descriptor creation.
// INPUT:  0 bytes
// OUTPUT: 1 byte - result code, 4 bytes - file descriptor
void ffitimer_create(unsigned char *c, long clen, unsigned char *a, long alen)
{
    assert(alen == 5);
    int fd = timerfd_create(CLOCK_REALTIME, TFD_NONBLOCK | TFD_CLOEXEC);
    if (0 <= fd)
    {
        a[0] = FFI_SUCCESS;
        int_to_byte4(fd, &a[1]);
    }
    else
        a[0] = FFI_FAILURE;
}

// Set Time in Timer file descriptor.
// INPUT:  4 bytes - file desctriptor, 8 bytes - initial timeout, 8 bytes - periodic timeout
// OUTPUT: 1 byte - result code
void ffitimer_set_time(unsigned char *c, long clen, unsigned char *a, long alen)
{
    assert(clen == 20);
    assert(alen == 1);

    int fd = byte4_to_int(c);
    long long a_initial_mcsec = byte8_to_int(&c[4]);
    long long a_period_mcsec = byte8_to_int(&c[12]);

    time_t initial_sec = a_initial_mcsec / 1000000;
    time_t initial_nsec = (a_initial_mcsec % 1000000) * 1000;
    time_t period_sec = a_period_mcsec / 1000000;
    time_t period_nsec = (a_period_mcsec % 1000000) * 1000;

    struct itimerspec timeout;
    timeout.it_value.tv_sec = initial_sec;
    timeout.it_value.tv_nsec = initial_nsec;
    timeout.it_interval.tv_sec = period_sec;
    timeout.it_interval.tv_nsec = period_nsec;

    int status = timerfd_settime(fd, 0, &timeout, NULL);

    if (0 == status)
        a[0] = FFI_SUCCESS;
    else
        a[0] = FFI_FAILURE;
}