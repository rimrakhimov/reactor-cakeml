#define _GNU_SOURCE

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "include/codes.h"
#include "include/marshalling.h"

// Socket creation.
// INPUT:  0 bytes.
// OUTPUT: 1 byte - result code, 4 bytes - file descriptor.
void ffisocket_create(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(alen == 5);
    int fd = socket(AF_INET, SOCK_STREAM | SOCK_NONBLOCK | SOCK_CLOEXEC, 0);
    if (0 <= fd) {
        a[0] = FFI_SUCCESS;
        int_to_byte4(fd, &a[1]);
    } else
        a[0] = FFI_FAILURE;
}

// Initiate a connection on a Socket.
// INPUT: 4 bytes - socket descriptor, 2 bytes - port, next is address string: example `127.0.0.1`.
// OUTPUT: 1 byte - result code.
void ffisocket_connect(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(6 < clen);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_port = byte2_to_int(&c[4]);

    struct in_addr inaddr;
    if (inet_aton(&c[6], &inaddr) == 0) {
        a[0] = FFI_FAILURE;
        return;
    }

    struct sockaddr_in addr_to_connect;
    memset(&addr_to_connect, 0, sizeof(addr_to_connect));

    addr_to_connect.sin_family = AF_INET;
    addr_to_connect.sin_addr = inaddr;
    addr_to_connect.sin_port = htons(c_port);

    if (connect(c_fd, (struct sockaddr *)&addr_to_connect, sizeof(addr_to_connect)) == 0) {
        a[0] = FFI_SUCCESS;
    } else {
        if (errno == EAGAIN || errno == EINPROGRESS) {
            a[0] = FFI_EAGAIN;
        } else if (errno == EINTR) {
            a[0] == FFI_EINTR;
        } else {
            a[0] = FFI_FAILURE;
        }
    }
}

// Bind Socket.
// INPUT:  4 bytes - socket descriptor, 2 bytes - port, next is address string: example '127.0.0.1'.
// OUTPUT: 1 byte - result code.
void ffisocket_bind(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(6 < clen);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_port = byte2_to_int(&c[4]);

    struct in_addr inaddr;
    if (inet_aton(&c[6], &inaddr) == 0) {
        a[0] = FFI_FAILURE;
        return;
    }

    struct sockaddr_in addr_to_bind;
    memset(&addr_to_bind, 0, sizeof(addr_to_bind));

    addr_to_bind.sin_family = AF_INET;
    addr_to_bind.sin_addr = inaddr;
    addr_to_bind.sin_port = htons(c_port);

    if (bind(c_fd, (struct sockaddr *)&addr_to_bind, sizeof(addr_to_bind)) < 0) {
        a[0] = FFI_FAILURE;
    } else {
        a[0] = FFI_SUCCESS;
    }
}

// Mark Socket as listening.
// INPUT:  4 bytes - socket descriptor, 4 bytes - maximum number of pending connections.
// OUTPUT: 1 byte - result code.
void ffisocket_listen(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 8);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_max_conn = byte4_to_int(&c[4]);

    if (listen(c_fd, c_max_conn) < 0)
        a[0] = FFI_FAILURE;
    else
        a[0] = FFI_SUCCESS;
}

// Mark Socket as listening.
// INPUT:  4 bytes - listening socket descriptor.
// OUTPUT: 1 byte - result code, 4 bytes - connected socket descriptor,
//         2 bytes - connected peer port, 1 byte - ip address string length,
//         next is connected peer address (up to 15 bytes).
void ffisocket_accept(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 4);
    assert(alen == 23);

    int c_acceptor_fd = byte4_to_int(c);

    int client_fd;
    struct sockaddr_in peer_sockaddr;
    socklen_t size = sizeof(peer_sockaddr);
    if ((client_fd = accept4(c_acceptor_fd,
                             (struct sockaddr *)&peer_sockaddr,
                             &size,
                             SOCK_NONBLOCK | SOCK_CLOEXEC)) >= 0) {
        char peer_addr[INET_ADDRSTRLEN];
        if (inet_ntop(AF_INET, &peer_sockaddr.sin_addr, peer_addr, INET_ADDRSTRLEN) != NULL) {
            a[0] = FFI_SUCCESS;
            int_to_byte4(client_fd, &a[1]);
            int_to_byte2(ntohs(peer_sockaddr.sin_port), &a[5]);

            a[7] = strlen(peer_addr);
            strncpy(&a[8], peer_addr, a[7]);
        } else {
            a[0] = FFI_FAILURE;
        }
    } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
        a[0] = FFI_EAGAIN;
    } else if (errno == EINTR) {
        a[0] = FFI_EINTR;
    } else {
        a[0] = FFI_FAILURE;
    }
}

// Sets TCP_NODELAY option for the Socket.
// INPUT:  4 bytes - socket descriptor, 1 byte - value to be set.
// OUTPUT: 1 byte - result code.
void ffisocket_set_tcp_nodelay(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 5);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_to_set = c[4];

    if (setsockopt(c_fd, SOL_TCP, TCP_NODELAY, &c_to_set, sizeof(c_to_set)) < 0)
        a[0] = FFI_FAILURE;
    else
        a[0] = FFI_SUCCESS;
}

// Sets SO_KEEPALIVE option for the Socket.
// INPUT:  4 bytes - socket descriptor, 1 byte - value to be set.
// OUTPUT: 1 byte - result code.
void ffisocket_set_so_keepalive(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 5);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_to_set = c[4];

    if (setsockopt(c_fd, SOL_SOCKET, SO_KEEPALIVE, &c_to_set, sizeof(c_to_set)) < 0)
        a[0] = FFI_FAILURE;
    else
        a[0] = FFI_SUCCESS;
}

// Sets TCP_KEEPIDLE value for the Socket.
// INPUT:  4 bytes - socket descriptor, 4 bytes - value to be set.
// OUTPUT: 1 byte - result code.
void ffisocket_set_tcp_keepidle(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 8);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_value = byte4_to_int(&c[4]);

    if (setsockopt(c_fd, SOL_TCP, TCP_KEEPIDLE, &c_value, sizeof(c_value)) < 0)
        a[0] = FFI_FAILURE;
    else
        a[0] = FFI_SUCCESS;
}

// Sets TCP_KEEPINTVL value for the Socket.
// INPUT:  4 bytes - socket descriptor, 4 bytes - value to be set.
// OUTPUT: 1 byte - result code.
void ffisocket_set_tcp_keepintvl(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 8);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_value = byte4_to_int(&c[4]);

    if (setsockopt(c_fd, SOL_TCP, TCP_KEEPINTVL, &c_value, sizeof(c_value)) < 0)
        a[0] = FFI_FAILURE;
    else
        a[0] = FFI_SUCCESS;
}

// Sets TCP_KEEPCNT value for the Socket.
// INPUT:  4 bytes - socket descriptor, 4 bytes - value to be set.
// OUTPUT: 1 byte - result code.
void ffisocket_set_tcp_keepcnt(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 8);
    assert(alen == 1);

    int c_fd = byte4_to_int(c);
    int c_value = byte4_to_int(&c[4]);

    if (setsockopt(c_fd, SOL_TCP, TCP_KEEPCNT, &c_value, sizeof(c_value)) < 0)
        a[0] = FFI_FAILURE;
    else
        a[0] = FFI_SUCCESS;
}
