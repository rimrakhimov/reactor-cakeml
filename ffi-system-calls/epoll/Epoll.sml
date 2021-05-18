structure Epoll =
struct
    (*
     *  Initializes an epoll mechanism for further usage.
     *
     *  @returns `int` epoll file descriptor 
     * 
     *  @raises `FFIFailure` if `epoll_create` syscall fails.
     *)
    fun create () =
        let
            val inbuf = ""
            val outbuf = Word8Array.array (1 + 4) (Word8.fromInt 0)
        in
            #(epoll_create) inbuf outbuf;
            FFIHelper.validate_status outbuf;
            MarshallingHelp.w42n outbuf 1
        end

    (*
     *  Is used to add, modify, or remove entries in the
     *  interest list of the epoll instance.
     *
     *  @param op_code `epoll_op_code`: operation to be performed.
     *  @param epoll_fd `int`: the file descriptor referring to epoll
     *      instance.
     *  @param fd `int`: target file descriptor.
     *  @param events_mask `epoll_events_mask`: an epoll events mask 
     *      file descriptor should be linked with.
     *
     *  @raises `FFIFailure` if `epoll_ctl` syscall fails.
     *)
    fun ctl (op_code : epoll_op_code) (epoll_fd : int) 
            (fd : int) (events_mask : epoll_events_mask) =
        let
            val op_code_bytes = EpollOpCode.to_bytes op_code
            val epoll_fd_bytes = MarshallingHelp.n2w4 epoll_fd
            val fd_bytes = MarshallingHelp.n2w4 fd
            val events_mask_bytes = EpollEventsMask.to_bytes events_mask

            val inbuf = ByteArray.concat_all 
                [op_code_bytes, epoll_fd_bytes, fd_bytes, events_mask_bytes]
            val outbuf = ByteArray.empty 1
        in
            #(epoll_ctl) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (*
     *  Waits for events on the epoll instance.
     *
     *  @param epoll_fd `int`: file descriptor referring to 
     *      epoll instance.
     *  @param max_events `int`: the maximum number of file descriptors
     *      the function is allowed to return.
     *  @param timeout `int`: the number of milliseconds that
     *      the function will block. Specifying a timeuot of ~1
     *      causes wait to block indefinitely.
     *
     *  @returns `epoll_event list` a list with file descriptors and
     *      corresponding masks with events occurred on them.
     *
     *  @raises `FFIEintr` if the syscall was interrupted by a signal.
     *  @raises `FFIFailure` if `epoll_ctl` syscall fails.               
     *)
    fun wait (epoll_fd : int) (max_events : int) (timeout : int) = 
        let
            val epoll_fd_bytes = MarshallingHelp.n2w4 epoll_fd
            val max_events_bytes = MarshallingHelp.n2w4 max_events
            val timeout_bytes = MarshallingHelp.n2w8 timeout

            val inbuf = ByteArray.concat_all 
                [epoll_fd_bytes, max_events_bytes, timeout_bytes]
            val outbuf = ByteArray.empty (1 + 4 + max_events * EpollEvent.size)

            val _ = #(epoll_wait) (ByteArray.to_string inbuf) outbuf
            val _ = FFIHelper.validate_status outbuf

            val n_events = MarshallingHelp.w42n outbuf 1
        in
            MarshallingHelp.bytes_to_list
                (fn x => fn y => (EpollEvent.from_bytes x y, EpollEvent.size))
                n_events
                outbuf
                5
        end
end
