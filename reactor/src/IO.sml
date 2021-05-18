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
     *  lable into a provided buffer.
     *)
    fun read_until_eagain (fd : int) (buff : io_buffer) =
        let
            fun internal (fd : int) (buff : io_buffer) (n_total : int) =
                let
                    val _ = IOBuffer.crunch buff (* Crunch the buffer before using. *)
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
                        (* There is still data available to be read. *)
                        internal fd buff (n_total + Word8Array.length data)
                    ) else if
                        status = FFICodes.eagain
                    then (* No more data available - return the total amount read. *)
                        n_total
                    else if 
                        status = FFICodes.eintr
                    then (* The syscall was interrupted. No data has been read. Try again. *)
                        internal fd buff n_total
                    else (* Any other error occurred while reading. *)
                        raise FFIFailure
                end
        in
            internal fd buff 0
        end
end
