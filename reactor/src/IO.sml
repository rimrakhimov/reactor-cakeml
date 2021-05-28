(**
 *  Is raised when there is no more space left in the
 *  provided buffer, but there is still data to read 
 *  in the kernel.
 *)
exception IOBufferOverflow

(**
 *  Is raised for pipes and FIFOs when trying to read,
 *  when there is no one writing to the other side.
 *  For sockets is raised when trying to read,
 *  when the other side closed its connection.
 *)
exception IOEndOfFile

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
     *  @param on_read `'a reactor -> int -> io_buffer -> 'a reactor * int`: 
     *      a callback function that is called when new data is read.
     *  @param has_fd_info `'a reactor -> int -> bool`: a function checking
     *      whether the file descriptor is still in the reactor.
     *
     *  @param returns `('a reactor * int, 'a reactor * int * exn) result`:
     *      if no error occurs, returns Ok consisting of a new reactor
     *      and number of bytes read. If any error occurs, returns Error consisting
     *      of reactor state and number of bytes read before the error,
     *      as well as an exception according to the error: `FFIFailure`, 
     *      `IOBufferOverflow`, or `IOEndOfFile`.
     *)
    fun read_until_eagain reactor (fd : int) (buff : io_buffer) on_read has_fd_info =
        let
            fun internal reactor (n_total : int) =
                if 
                    not (has_fd_info reactor fd)
                then
                    (* The descriptor has been removed from the reactor somewhere
                     * during read_handler processing of income data. Stop reading
                     * the data and return current reactor state. We do not cosider
                     * that as an error. *)
                    Ok (reactor, n_total)
                else
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
                        if 
                            space = 0 
                        then
                            (*  As space was 0, and the `read` call did not return EAGAIN,
                             *  there is still data in the buffer that may be read, but cannot
                             *  as buffer is overflowed.
                             *) 
                            Error (reactor, n_total, IOBufferOverflow)
                        else if 
                            Word8Array.length data = 0
                        then
                            (*  As the space was greater than zero, but the `read` syscall read
                             *  zero bytes, the other side of the descriptor was closed.
                             *)
                            Error (reactor, n_total, IOEndOfFile)
                        else (
                            IOBuffer.write buff data;
                            let
                                val (new_reactor, consumed) = on_read reactor fd buff
                            in
                                (* If some data has been consumed, remove it from the buffer and
                                 * crunch it if size left is less than lower watermark. *)
                                if consumed > 0
                                then IOBuffer.consume_and_crunch buff consumed
                                else ();
                                
                                (* There is still data available to be read. *)
                                internal new_reactor (n_total + Word8Array.length data)
                            end
                        )
                    ) else if
                        status = FFICodes.eagain
                    then (* No more data available - return the total amount read. *)
                        Ok (reactor, n_total)
                    else if 
                        status = FFICodes.eintr
                    then (* The syscall was interrupted. No data has been read. Try again. *)
                        internal reactor n_total
                    else (* Any other error occurred while reading. *)
                        Error (reactor, n_total, FFIFailure)
                end
        in
            internal reactor 0
        end

    fun write_until_eagain (fd : int) (data : byte_array) (buff : io_buffer) =
        let
            val chunk_size = 1024 * 1024 (* Write maximum 1Mb of data at one call. *)

            val initial_buff_size = IOBuffer.size buff
            val data_size = Word8Array.length data

            fun write_into_buffer (buff : io_buffer) (data : byte_array) = (
                (* If there is enough space inside the buffer,
                 * save data inside. If not, raise corresponding exception. *)
                if IOBuffer.size buff < Word8Array.length data
                then raise IOBufferOverflow
                else IOBuffer.write buff data
            )

            fun write_buffer (buff : io_buffer) =
                if IOBuffer.empty buff
                then Ok ()
                else
                    let
                        val msg = IOBuffer.read buff (min chunk_size (IOBuffer.size buff))
                        val res = Ok (Fd.write fd (ByteArray.from_string msg))
                        handle exn => Error exn
                    in
                        case res of
                            Ok n => (
                                IOBuffer.consume_and_crunch buff n;
                                write_buffer buff
                            )
                          | Error FFIEintr => 
                                (* The syscall was interrupted. Try writing again. *)
                                write_buffer buff
                          | Error e => Error e (* FFIEagain or FFIFailure. *)
                    end

            fun write_data (data : byte_array) n_total =
                let
                    val rem = Word8Array.length data - n_total
                in
                    if rem = 0
                    then ()
                    else
                        let
                            val to_write = ByteArray.subarray data n_total (min rem chunk_size)
                            val res = Ok (Fd.write fd to_write)
                            handle exn => Error exn
                        in
                            case res of
                                Ok n => write_data data (n_total + n)
                              | Error FFIEintr => write_data data n_total
                              | Error FFIEagain =>
                                    (* We cannot write remaining data. Save it 
                                     * into the buffer. *)
                                    write_into_buffer buff (ByteArray.subarray data n_total rem)
                              | Error FFIFailure => raise FFIFailure

                        end
                end

            val write_buffer_res = write_buffer buff
        in
            case write_buffer_res of
                Ok () => write_data data 0
              | Error FFIEagain => (
                    (* We cannot write data right now. Just 
                     * save it into the buffer *)
                    write_into_buffer buff data
                )
              | Error FFIFailure => raise FFIFailure;
            
            (* Calculate how many bytes have been written. *)
            (initial_buff_size + data_size) - (IOBuffer.size buff)
        end

    (* fun write_until_eagain reactor (data : string) (buff : io_buffer) =
        let
            fun internal (data : string) (n_total : int) =
                let
                    val rem = String.size data
                in
                  ()
                end

            fun write_buffer (buff : io_buffer) =
                        let
                        in
                            if IOBuffer.empty buff
                            then ()
                            else ()
                        end
        in
            internal data 0
        end *)
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
