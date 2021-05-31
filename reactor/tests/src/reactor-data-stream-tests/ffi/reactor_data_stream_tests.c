#include <arpa/inet.h>
#include <assert.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "../../../../../ffi-system-calls/ffi/include/codes.h"
#include "../../../../../ffi-system-calls/ffi/include/marshalling.h"

void ffitest_data_stream_can_connect_to_acceptor(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 2);

    pid_t childpid;

    childpid = fork();
    if (childpid == -1) {
        perror("Forking failed");
        exit(EXIT_FAILURE);
    }

    if (childpid == 0) { /* The child process creates a listening socket */
        int c_port = byte2_to_int(c);

        int acceptor_fd = socket(AF_INET, SOCK_STREAM, 0);

        struct in_addr inaddr;
        if (inet_aton("127.0.0.1", &inaddr) == 0) {
            perror("inet_aton() failed");
            exit(EXIT_FAILURE);
        }

        struct sockaddr_in addr_to_bind;
        memset(&addr_to_bind, 0, sizeof(addr_to_bind));

        addr_to_bind.sin_family = AF_INET;
        addr_to_bind.sin_addr = inaddr;
        addr_to_bind.sin_port = htons(c_port);

        if (bind(acceptor_fd, (struct sockaddr *)&addr_to_bind, sizeof(addr_to_bind)) < 0) {
            perror("Binding failed");
            exit(EXIT_FAILURE);
        }

        if (listen(acceptor_fd, 1) < 0) {
            perror("Listening failed");
            exit(EXIT_FAILURE);
        }

        int client_fd;
        struct sockaddr_in peer_sockaddr;
        socklen_t size = sizeof(peer_sockaddr);
        if ((client_fd = accept(acceptor_fd, (struct sockaddr *)&peer_sockaddr, &size)) < 0) {
            perror("Accepting failed");
            exit(EXIT_FAILURE);
        }
        close(acceptor_fd);

        sleep(1);

        close(client_fd);

        exit(EXIT_SUCCESS);
    } else {
    }
}

void ffitest_data_stream_can_read_incoming_message(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 6);

    char text[50] = "TESTING DATA. SHOULD BE READ IN NON-BLOCKING MODE";

    pid_t childpid;

    childpid = fork();
    if (childpid == -1) {
        perror("Forking failed");
        exit(EXIT_FAILURE);
    }

    if (childpid == 0) { /* The child process creates a listening socket */
        int c_port = byte2_to_int(c);
        
        int c_fd = byte4_to_int(&c[2]);
        close(c_fd);

        int acceptor_fd = socket(AF_INET, SOCK_STREAM, 0);

        struct in_addr inaddr;
        if (inet_aton("127.0.0.1", &inaddr) == 0) {
            perror("inet_aton() failed");
            exit(EXIT_FAILURE);
        }

        struct sockaddr_in addr_to_bind;
        memset(&addr_to_bind, 0, sizeof(addr_to_bind));

        addr_to_bind.sin_family = AF_INET;
        addr_to_bind.sin_addr = inaddr;
        addr_to_bind.sin_port = htons(c_port);

        if (bind(acceptor_fd, (struct sockaddr *)&addr_to_bind, sizeof(addr_to_bind)) < 0) {
            perror("Binding failed");
            exit(EXIT_FAILURE);
        }

        if (listen(acceptor_fd, 1) < 0) {
            perror("Listening failed");
            exit(EXIT_FAILURE);
        }

        int client_fd;
        struct sockaddr_in peer_sockaddr;
        socklen_t size = sizeof(peer_sockaddr);
        if ((client_fd = accept(acceptor_fd, (struct sockaddr *)&peer_sockaddr, &size)) < 0) {
            perror("Accepting failed");
            exit(EXIT_FAILURE);
        }
        close(acceptor_fd);

        sleep(1);
        write(client_fd, &text[0], 12);
        sleep(3);
        write(client_fd, &text[12], 24);
        sleep(2);
        write(client_fd, &text[36], 13);
        sleep(1);

        close(client_fd);

        exit(EXIT_SUCCESS);
    } else {
        sleep(1);
    }
}

void ffitest_read_write_data_stream_can_write_and_read(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(clen == 2);

    pid_t childpid;

    childpid = fork();
    if (childpid == -1) {
        perror("Forking failed");
        exit(EXIT_FAILURE);
    }

    if (childpid == 0) { /* The child process creates a listening socket */
        int c_port = byte2_to_int(c);

        int acceptor_fd = socket(AF_INET, SOCK_STREAM, 0);

        struct in_addr inaddr;
        if (inet_aton("127.0.0.1", &inaddr) == 0) {
            perror("inet_aton() failed");
            exit(EXIT_FAILURE);
        }

        struct sockaddr_in addr_to_bind;
        memset(&addr_to_bind, 0, sizeof(addr_to_bind));

        addr_to_bind.sin_family = AF_INET;
        addr_to_bind.sin_addr = inaddr;
        addr_to_bind.sin_port = htons(c_port);

        if (bind(acceptor_fd, (struct sockaddr *)&addr_to_bind, sizeof(addr_to_bind)) < 0) {
            perror("Binding failed");
            exit(EXIT_FAILURE);
        }

        if (listen(acceptor_fd, 1) < 0) {
            perror("Listening failed");
            exit(EXIT_FAILURE);
        }

        int client_fd;
        struct sockaddr_in peer_sockaddr;
        socklen_t size = sizeof(peer_sockaddr);
        if ((client_fd = accept(acceptor_fd, (struct sockaddr *)&peer_sockaddr, &size)) < 0) {
            perror("Accepting failed");
            exit(EXIT_FAILURE);
        }
        close(acceptor_fd);

        int n = -1;
        char buff[100];
        while ((n = read(client_fd, buff, 100)) > 0) {
            sleep(3);
            write(client_fd, buff, n);
        }

        sleep(1);

        close(client_fd);

        exit(EXIT_SUCCESS);
    } else {
        sleep(1);
    }
}