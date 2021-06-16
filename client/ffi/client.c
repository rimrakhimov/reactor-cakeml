#include <stdio.h>

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Invalid number of arguments. Expected 1 (server port number), got: %d\n", argc);
    }
    return 0;
}