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
            val inbuf = MarshallingHelp.n2w4 fd
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
            val fd_bytes = MarshallingHelp.n2w4 fd
            val count_bytes = MarshallingHelp.n2w4 count

            val inbuf = ByteArray.concat_all [fd_bytes, count_bytes]
            val outbuf = ByteArray.empty (1 + 4 + count)

            val _ = #(fd_read) (ByteArray.to_string inbuf) outbuf
            val _ = FFIHelper.validate_status outbuf
            
            val n = MarshallingHelp.w42n outbuf 1
        in
            ByteArray.subarray outbuf 5 n
        end
end