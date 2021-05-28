structure Socket =
struct
    (**
     *  Creates an endpoint for communication and returns a file
     *  descriptor that refers to that endpoint.
     *
     *  @returns `int`: a file descriptor that refers to that endpoint.
     *
     *  @raises `FFIFailure` if `socket` syscall fails.
     *)
    fun create () =
        let
            val outbuf = ByteArray.empty (1 + 4)
        in
            #(socket_create) "" outbuf;
            FFIHelper.validate_status outbuf;
            MarshallingHelp.w42n outbuf 1
        end

    (**
     *  Initiate a connection on a socket reffered to
     *  by the file descriptor.
     *
     *  @param fd `int`: the file descriptor that refers to the socket.
     *  @param address `in_addr`: a IPv4 address where the socket 
     *      should initiate connection to.
     *  @param port `int`: a port number referring to the socket where connection 
     *      should be initiated to.
     * 
     *  @raises `FFIFailure` if connection syscalls fail.
     *  @raises `FFIEagain` if file descriptor refers to nonblocking
     *      socket and the connection cannot be completed immediately.
     *  @raises `FFIEintr` if `connect` syscall was interrupted by a signal.
     *)
    fun connect (fd : int) (address : in_addr) (port : int) =
        let
            val address_bytes = ByteArray.from_string (InAddr.to_string address)

            val inbuf = ByteArray.empty (4 + 2 + Word8Array.length address_bytes)
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = Marshalling.n2w2 port inbuf 4
            val _ = Word8Array.copy address_bytes 0 (Word8Array.length address_bytes) inbuf 6
            
            val outbuf = ByteArray.empty 1
        in
            #(socket_connect) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (**
     *  Assigns the address to the socket referred to
     *  by the file descriptor.
     *
     *  @param fd `int`: the file descriptor that refers to the socket.
     *  @param address `in_addr`: a IPv4 address which the socket should be binded to.
     *  @param port `int`: a port number where the socket should be binded to.
     * 
     *  @raises `FFIFailure` if binding syscalls fail.
     *)
    fun bind (fd : int) (address : in_addr) (port : int) =
        let
            val address_bytes = ByteArray.from_string (InAddr.to_string address)

            val inbuf = ByteArray.empty (4 + 2 + Word8Array.length address_bytes)
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = Marshalling.n2w2 port inbuf 4
            val _ = Word8Array.copy address_bytes 0 (Word8Array.length address_bytes) inbuf 6
            
            val outbuf = ByteArray.empty 1
        in
            #(socket_bind) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (**
     *  Mark the socket referred to the file descriptor
     *  as a listening socket.
     *
     *  @param fd `int`: the file descriptor referring to a socket
     *      to be listened.
     *  @param backlog `int`: the maximum length to which the
     *      queue of pending connections may grow.
     *
     *  @raises `FFIFailure` if `listen` syscall fails.
     *)
    fun listen (fd : int) (backlog : int) =
        let
            val inbuf = ByteArray.empty 8
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = MarshallingHelp.n2w4 backlog inbuf 4

            val outbuf = ByteArray.empty 1
        in
            #(socket_listen) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (**
     *  Accepts a new pending connection, creates a new connected socket,
     *  and returns a new file descriptor referring to that socket.
     *
     *  @oaram acceptor_fd `int`: a descriptor referring to listening socket.
     *
     *  @returns (int * in_addr * int): a descriptor referring to a new connected
     *      socket, IPv4 address and a port of the peer socket.
     *
     *  @raises `FFIFailure` if `accept` syscall fails.
     *  @raises `FFIEagain` if acceptor file descriptor refers to nonblocking
     *      socket and there is no more pending connections in the queue.
     *  @raises `FFIEintr` if `accept` syscall was interrupted by a signal.
     *)
    fun accept (acceptor_fd : int) = 
        let
            val inbuf = ByteArray.empty 4
            val _ = MarshallingHelp.n2w4 acceptor_fd inbuf 0

            val outbuf = ByteArray.empty 23
        in
            #(socket_accept) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            let
                val client_fd = MarshallingHelp.w42n outbuf 1
                val client_port = Marshalling.w22n outbuf 5
                val client_address_len = Word8.toInt (Word8Array.sub outbuf 7)
                val client_address = 
                    InAddr.from_string (Word8Array.substring outbuf 8 client_address_len)
            in
                (client_fd, client_address, client_port)
            end
        end

    (**
     *  Enables or disables TCP_NODELAY option for the specified socket.
     *  TCP_NODELAY option if set, disable the Nagle algorithm.
     *
     *  @param fd `int`: a file descriptor that refers to the socket.
     *  @param to_set `bool`: if True, the option is enabled; if False,
     *      the option is disabled.
     *
     *  @raises `FFIFailure` if `setsockopt` syscall fails.
     *)
    fun set_tcp_nodelay (fd : int) (to_set : bool) =
        let
            val inbuf = ByteArray.empty 5
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = Word8Array.update inbuf 4 (Word8.fromInt (if to_set then 1 else 0))
            
            val outbuf = ByteArray.empty 1
        in
            #(socket_set_tcp_nodelay) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end
    
    (**
     *  Enables or disables SO_KEEPALIVE option for the specified socket.
     *  SO_KEEPALIVE option if set, enables sending of keep-alive messages,
     *  that in turn allow to detect if peer socket has been disconnected.
     *
     *  @param fd `int`: a file descriptor that refers to the socket.
     *  @param to_set `bool`: if True, the option is enabled; if False,
     *      the option is disabled.
     *
     *  @raises `FFIFailure` if `setsockopt` syscall fails.
     *)
    fun set_so_keepalive (fd : int) (to_set : bool) =
        let
            val inbuf = ByteArray.empty 5
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = Word8Array.update inbuf 4 (Word8.fromInt (if to_set then 1 else 0))
            
            val outbuf = ByteArray.empty 1
        in
            #(socket_set_so_keepalive) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (**
     *  Sets value for TCP_KEEPIDLE option. The option sets
     *  the time (in seconds) the connection needs to remain idle
     *  before TCP starts sending keepalive probes, if the socket
     *  option SO_KEEPALIVE has been set on this socket.
     *
     *  @param fd `int`: a file descriptor that refers to the socket.
     *  @param value `int`: the time (in seconds).
     *
     *  @raises `FFIFailure` if `setsockopt` syscall fails.
     *)
    fun set_tcp_keepidle (fd : int) (value : int) =
        let
            val inbuf = ByteArray.empty 8
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = MarshallingHelp.n2w4 value inbuf 4

            val outbuf = ByteArray.empty 1
        in
            #(socket_set_tcp_keepidle) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (**
     *  Sets value for TCP_KEEPINTVL option. The option sets
     *  The time (in seconds) between individual keepalive probes.
     *
     *  @param fd `int`: a file descriptor that refers to the socket.
     *  @param value `int`: the time (in seconds).
     *
     *  @raises `FFIFailure` if `setsockopt` syscall fails.
     *)
    fun set_tcp_keepintvl (fd : int) (value : int) =
        let
            val inbuf = ByteArray.empty 8
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = MarshallingHelp.n2w4 value inbuf 4

            val outbuf = ByteArray.empty 1
        in
            #(socket_set_tcp_keepintvl) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (**
     *  Sets value for TCP_KEEPCNT option. The maximum number of 
     *  keepalive probes TCP should send before dropping the connection.
     *
     *  @param fd `int`: a file descriptor that refers to the socket.
     *  @param value `int`: the time (in seconds).
     *
     *  @raises `FFIFailure` if `setsockopt` syscall fails.
     *)
    fun set_tcp_keepcnt (fd : int) (value : int) =
        let
            val inbuf = ByteArray.empty 8
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = MarshallingHelp.n2w4 value inbuf 4

            val outbuf = ByteArray.empty 1
        in
            #(socket_set_tcp_keepcnt) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (**
     *  Enables or disables SO_REUSEADDR option for the specified socket.
     *  Option if set, indicates that the rules used in validating addresses
     *  supplied in a bind(2) call should allow reuse of local addresses.
     *
     *  @param fd `int`: a file descriptor that refers to the socket.
     *  @param to_set `bool`: if True, the option is enabled; if False,
     *      the option is disabled.
     *
     *  @raises `FFIFailure` if `setsockopt` syscall fails.
     *)
    fun set_so_reuseaddr (fd : int) (to_set : bool) =
        let
            val inbuf = ByteArray.empty 5
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = Word8Array.update inbuf 4 (Word8.fromInt (if to_set then 1 else 0))
            
            val outbuf = ByteArray.empty 1
        in
            #(socket_set_so_reuseaddr) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end
end

(* val epoll_fd = Epoll.create ()

val listen_fd = Socket.create ()
val _ = Socket.bind listen_fd (InAddr.from_string "127.0.0.1") 12347
val _ = Socket.listen listen_fd 100

val fd = Socket.create()
val _ = Epoll.ctl EpollCtlAdd epoll_fd fd (EpollEventsMask.from_list [Epollet, Epollrdhup, Epollin, Epollout])
val _ = Socket.connect fd (InAddr.from_string "127.0.0.1") 12347
handle FFIEagain => print "\n\n===SOCKET_CONNECT: FFI_EAGAIN===\n\n"

val _ = print "\n\n===HELLO_1===\n\n"
val _ = Socket.accept listen_fd
val _ = print "\n\n===HELLO_2===\n\n"
val epoll_event_list = Epoll.wait epoll_fd 1 (~1)
val ev = List.hd epoll_event_list
val _ = print ("\n\n===EPOLL_EVENT: " ^ EpollEvent.to_string ev ^ "===\n\n") *)