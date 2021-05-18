#ifndef EPOLL_CODES_H
#define EPOLL_CODES_H

#define FFI_EPOLL_CTL_ADD       0
#define FFI_EPOLL_CTL_MOD       1
#define FFI_EPOLL_CTL_DEL       2

#define FFI_EPOLL_EPOLLIN       1
#define FFI_EPOLL_EPOLLOUT      2
#define FFI_EPOLL_EPOLLRDHUP    4
#define FFI_EPOLL_EPOLLPRI      8
#define FFI_EPOLL_EPOLLERR      16
#define FFI_EPOLL_EPOLLHUP      32
#define FFI_EPOLL_EPOLLET       64
#define FFI_EPOLL_EPOLLONESHOT  128

#endif
