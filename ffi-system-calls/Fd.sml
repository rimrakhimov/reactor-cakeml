structure Fd =
struct
    (**
     *  Closes the specified file descriptor.
     *
     *  @param fd `int`: fd to be closed.
     *
     *  @raises `FFIFailure` if `close` syscall fails. 
     *)
    fun close fd =
        let
            val inbuf = ByteArray.empty 4
            val _ = MarshallingHelp.n2w4 fd inbuf 0

            val outbuf = ByteArray.empty 1
        in
            #(fd_close) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end

    (**
     *  Read from a file descriptor.
     *
     *  @param fd `int`: a descriptor to read data from.
     *  @param count `int`: maxumum number of bytes to be read.
     *
     *  @returns `byte_array` the data that was read.
     *
     *  @raises `FFIEagain` if file descriptor refers to nonblocking
     *      file and `read` syscall would block.
     *  @raises `FFIEintr` if `read` syscall was interrupted by a signal.
     *  @raises `FFIFailure` if any other error occured during the call.
     *)
    fun read fd count =  
        let
            val inbuf = ByteArray.empty 8
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = MarshallingHelp.n2w4 count inbuf 4

            val outbuf = ByteArray.empty (1 + 4 + count)

            val _ = #(fd_read) (ByteArray.to_string inbuf) outbuf
            val _ = FFIHelper.validate_status outbuf
            
            val n = MarshallingHelp.w42n outbuf 1
        in
            ByteArray.subarray outbuf 5 n
        end

    (**
     *  Write data into a file descriptor.
     *
     *  @param fd `int`: a descriptor to write data into.
     *  @param data `byte_array`: data to be written.
     *
     *  @returns `int`: actual number of bytes written.
     *
     *  @raises `FFIEagain` if file descriptor refers to nonblocking
     *      file and `write` syscall would block.
     *  @raises `FFIEintr` if `write` syscall was interrupted by a signal.
     *  @raises `FFIFailure` if any other error occured during the call.
     *)
    fun write fd (data : byte_array) =
        let
<<<<<<< HEAD
            val fd_bytes = MarshallingHelp.n2w4 fd

            val inbuf = ByteArray.concat fd_bytes data
=======
            val inbuf = ByteArray.empty (4 + Word8Array.length data)
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = Word8Array.copy data 0 (Word8Array.length data) inbuf 4

>>>>>>> functional-style
            val outbuf = ByteArray.empty (1 + 4)
        in
            #(fd_write) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            MarshallingHelp.w42n outbuf 1
        end

<<<<<<< HEAD

=======
>>>>>>> functional-style
    (**
     *  Put file descriptor into blocking/nonblocking mode.
     *
     *  @param fd `int`: a descriptor to put the mode.
     *  @param blocking `bool`: if true the descriptor will
     *      be put into blocking mode, otherwise the descriptor
     *      will be put into nonblocking mode.
     *
     *  @raises `FFIFailure` if `fcntl` syscall fails with an error.
     *)
    fun set_blocking (fd : int) (blocking : bool) =
        let
<<<<<<< HEAD
            val fd_bytes = MarshallingHelp.n2w4 fd
            val blocking_bytes = Word8Array.array 1 (Word8.fromInt (if blocking then 1 else 0))
            
            val inbuf = ByteArray.concat_all [fd_bytes, blocking_bytes]
=======
            val inbuf = ByteArray.empty 5
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = Word8Array.update inbuf 4 (Word8.fromInt (if blocking then 1 else 0))
            
>>>>>>> functional-style
            val outbuf = ByteArray.empty 1
        in
            #(fd_set_blocking) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end
end
