#ifndef MARSHALLING_H
#define MARSHALLING_H

#include <stdint.h>

void int_to_byte8(int i, unsigned char *b);
int byte8_to_int(unsigned char *b);

static inline void int64_to_byte8(int64_t i, unsigned char *b) {
    b[0] = (i >> 56) & 0xFF;
    b[1] = (i >> 48) & 0xFF;
    b[2] = (i >> 40) & 0xFF;
    b[3] = (i >> 32) & 0xFF;
    b[4] = (i >> 24) & 0xFF;
    b[5] = (i >> 16) & 0xFF;
    b[6] = (i >> 8) & 0xFF;
    b[7] =  i & 0xFF;
}

static inline void int_to_byte4(int i, unsigned char *b) {
    /* i is encoded on 4 bytes */
    b[0] = (i >> 24) & 0xFF;
    b[1] = (i >> 16) & 0xFF;
    b[2] = (i >> 8) & 0xFF;
    b[3] = i & 0xFF;
    // printf("\n===b0=%d, b1=%d, b2=%d, b3=%d===\n", b[0], b[1], b[2], b[3]);
}

static inline int byte4_to_int(unsigned char *b) {
    return ((b[0] << 24) | (b[1] << 16) | (b[2] << 8) | b[3]);
}

#endif
