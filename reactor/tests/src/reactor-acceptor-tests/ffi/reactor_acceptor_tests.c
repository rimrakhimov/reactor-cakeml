#include <arpa/inet.h>
#include <assert.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "../../../../../ffi-system-calls/ffi/include/codes.h"
#include "../../../../../ffi-system-calls/ffi/include/marshalling.h"

void ffireactor_acceptor_tests_sleep(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 1);

    sleep(c[0]);
}

void ffitest_can_accept_connection(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 2);
    assert(alen == 0);

    pid_t childpid;

    childpid = fork();
    if (childpid == -1) {
        perror("Forking failed");
        exit(EXIT_FAILURE);
    }

    if (childpid == 0) { /* The child process tries to connect to a listening socket */
        int c_port = byte2_to_int(c);

        int fd = socket(AF_INET, SOCK_STREAM, 0);
        struct in_addr inaddr;
        if (inet_aton("127.0.0.1", &inaddr) == 0) {
            perror("inet_aton() failed");
            exit(EXIT_FAILURE);
        }

        struct sockaddr_in addr_to_connect;
        memset(&addr_to_connect, 0, sizeof(addr_to_connect));
        addr_to_connect.sin_family = AF_INET;
        addr_to_connect.sin_addr = inaddr;
        addr_to_connect.sin_port = htons(c_port);

        if (connect(fd, (struct sockaddr *)&addr_to_connect, sizeof(addr_to_connect)) < 0) {
            perror("Connecting failed");
            exit(EXIT_FAILURE);
        }

        sleep(1);

        close(fd);
        exit(EXIT_SUCCESS);
    } else {
    }
}

void ffitest_can_accept_several_connections(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 6);
    assert(alen == 0);

    pid_t childpid;

    childpid = fork();
    if (childpid == -1) {
        perror("Forking failed");
        exit(EXIT_FAILURE);
    }

    if (childpid == 0) { /* The child process tries to connect to a listening socket */
        int c_port = byte2_to_int(c);
        int conn_num = byte4_to_int(&c[2]);

        struct in_addr inaddr;
        if (inet_aton("127.0.0.1", &inaddr) == 0) {
            perror("inet_aton() failed");
            exit(EXIT_FAILURE);
        }

        int *socket_fds = malloc(conn_num * (sizeof(int)));
        if (socket_fds == NULL && conn_num > 0) {
            perror("Space allocating failed");
            exit(EXIT_FAILURE);
        }
        for (int i = 0; i < conn_num; i++) {
            int fd = socket(AF_INET, SOCK_STREAM, 0);
            socket_fds[i] = fd;

            struct sockaddr_in addr_to_connect;
            memset(&addr_to_connect, 0, sizeof(addr_to_connect));
            addr_to_connect.sin_family = AF_INET;
            addr_to_connect.sin_addr = inaddr;
            addr_to_connect.sin_port = htons(c_port);

            if (connect(fd, (struct sockaddr *)&addr_to_connect, sizeof(addr_to_connect)) < 0) {
                perror("Connecting failed");
                exit(EXIT_FAILURE);
            }
        }

        sleep(1);
        for (int i = 0; i < conn_num; i++) {
            close(socket_fds[i]);
        }


        exit(EXIT_SUCCESS);
    } else {
    }
}