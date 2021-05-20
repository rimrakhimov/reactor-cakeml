(**
 *  Is raised when there is no more space left in the
 *  provided buffer, but there is still data to read 
 *  in the kernel.
 *)
exception IOBufferOverflow

(**
 *  Structure defines functions to make input and output
 *  operations with the kernel buffer.
 *)
structure IO =
struct
    (**
     *  Reads in a data from a non-blocking FD while data are avai-
     *  lable into a provided buffer. After every return from a `read`
     *  syscall read_handler is called, and used number of bytes are
     *  consumed from the buffer.
     *
     *  @param reactor `'a reactor`: a reactor that should be passed as an
     *      argument into the read_handler.
     *  @param fd `int`: a file descriptor to read data from.
     *  @param buff `io_buffer`: a buffer where data should be read into.
     *  @param on_read `'a reactor read_handler`: a callback function that is
     *      called when new data is read.
     *
     *  @raises `FFIFailure` if `read` syscall fails with any unexpected error.
     *)
    fun read_until_eagain reactor (fd : int) (buff : io_buffer) on_read =
        let
            fun internal (n_total : int) =
                let
                    val space = IOBuffer.capacity buff

                    val (data, status) = (Fd.read fd space, FFICodes.success)
                    handle
                        FFIEagain => (ByteArray.empty 0, FFICodes.eagain)
                      | FFIEintr => (ByteArray.empty 0, FFICodes.eintr)
                      | FFIFailure => (ByteArray.empty 0, FFICodes.failure)
                in
                    if 
                        status = FFICodes.success
                    then (
                        (*  As space was 0, and the `read` call did not return EAGAIN,
                         *  there is still data in the buffer that may be read, but cannot
                         *  as buffer is overflowed.
                         *)
                        if space = 0 then raise IOBufferOverflow else ();

                        IOBuffer.write buff data;
                        let
                            val consumed = on_read reactor fd (IOBuffer.read buff (IOBuffer.size buff))
                        in
                            (* If some data has been consumed, remove it from the buffer and
                             * crunch it if size left is less than lower watermark. *)
                            if consumed > 0
                            then IOBuffer.consume_and_crunch buff consumed
                            else ()
                        end;
                        (* There is still data available to be read. *)
                        internal (n_total + Word8Array.length data)
                    ) else if
                        status = FFICodes.eagain
                    then (* No more data available - return the total amount read. *)
                        n_total
                    else if 
                        status = FFICodes.eintr
                    then (* The syscall was interrupted. No data has been read. Try again. *)
                        internal n_total
                    else (* Any other error occurred while reading. *)
                        raise FFIFailure
                end
        in
            internal 0
        end
end

local
    fun exn_printer e =
        case e of
            IOBufferOverflow => "IOBufferOverflow"
          | _ => raise Exception.Unknown
in
    val _ = Exception.add_exn_name_printer exn_printer
    val _ = Exception.add_exn_message_printer exn_printer
end
