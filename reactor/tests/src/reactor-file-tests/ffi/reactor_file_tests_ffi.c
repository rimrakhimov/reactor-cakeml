#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "../../../../../ffi-system-calls/ffi/include/codes.h"
#include "../../../../../ffi-system-calls/ffi/include/marshalling.h"

void ffitest_file_read_non_blocking(unsigned char *c, long clen, unsigned char *a, long alen) {
    assert(alen == 4);

    char text[50] = "TESTING DATA. SHOULD BE READ IN NON-BLOCKING MODE";

    int pipefd[2];
    pid_t childpid;

    if (pipe(pipefd) == -1) {
        perror("Pipe creation failed");
        exit(EXIT_FAILURE);
    }

    int flags;
    if ((flags = fcntl(pipefd[0], F_GETFL)) < 0) {
        perror("Getting flags of a read end of the pipe failed");
        exit(EXIT_FAILURE);
    }
    if ((fcntl(pipefd[0], F_SETFL, flags || O_NONBLOCK)) < 0) {
        perror("Setting nonblocking flag of a read end of the pipe failed");
        exit(EXIT_FAILURE);
    }

    childpid = fork();
    if (childpid == -1) {
        perror("Forking failed");
        exit(EXIT_FAILURE);
    }

    if (childpid == 0) {    /* The child process writes to a pipe periodically */
        close(pipefd[0]);   /* Close unused read end */

        sleep(2);
        write(pipefd[1], &text[0], 12);
        sleep(5);
        write(pipefd[1], &text[12], 24);
        sleep(4);
        write(pipefd[1], &text[36], 13);
        sleep(2);

        close(pipefd[1]);   /* Close write end after everything has been written */
        exit(EXIT_SUCCESS);
    } else { /* The parent process returns read end of the pipe to CakeML */
        close(pipefd[1]);   /* Close unused write end */

        int_to_byte4(pipefd[0], a);
    }
}