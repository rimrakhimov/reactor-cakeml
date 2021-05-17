#ifndef MARSHALLING_H
#define MARSHALLING_H

void int_to_byte8(int i, unsigned char *b);
int byte8_to_int(unsigned char *b);

static inline void int_to_byte4(int i, unsigned char *b)
{
    /* i is encoded on 4 bytes */
    b[0] = (i >> 24) & 0xFF;
    b[1] = (i >> 16) & 0xFF;
    b[2] = (i >> 8) & 0xFF;
    b[3] = i & 0xFF;
}

static inline int byte4_to_int(unsigned char *b)
{
    return ((b[0] << 24) | (b[1] << 16) | (b[2] << 8) | b[3]);
}

#endif
