structure Fd =
struct
    (**
     *  Closes the specified file descriptor.
     *
     *  @param fd `int`: fd to be closed.
     *
     *  @raises FFIFailure if `close` syscall fails. 
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
end
