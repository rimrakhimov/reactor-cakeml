(*  The type represents internal representation of the buffer 
 *  @param buf `byte_array ref`: reference to the buffer's data
 *  @param rd_offset `int ref`: reference to the current buffer's read offset
 *  @param wr_offset `int ref`: reference to the current buffer's write offset
 *  @param wr_lwm `int`: the low-memory watermark that should be set to
 *      exceed the maximum expected message size to be stored
 *      in the buffer. When the buffer has less than that amount
 *      of space left, it'll call crunch() function.
 *)
datatype io_buffer = IOBuffer (byte_array ref) (int ref) (int ref) int

(*  Is raised if low watermark set exceeds the size of a buffer
 *
 *  @param size `int`: current maximal size of the buffer
 *  @param low_watermark `int`: the watermark tried to be set
 *)
exception IOBufferLowWatermarkTooHigh int int

(*  Is raised when trying to read from a buffer more 
 *   than the buffer contains.
 *
 *  @param size `int`: current number of bytes available to read
 *  @param offset `int`: the offset from which data tried to be read
 *  @param n `int`: number of bytes tried to be read
 *)
exception IOBufferNotEnoughData int int int

structure IOBufferType =
struct
    fun get_buffer_ref (IOBuffer buf _ _ _) = buf
    fun get_rd_offset_ref (IOBuffer _ rd_offset _ _) = rd_offset
    fun get_wr_offset_ref (IOBuffer _ _ wr_offset _) = wr_offset

    fun get_buffer_val iob = !(get_buffer_ref iob)
    fun get_rd_offset_val iob = !(get_rd_offset_ref iob)
    fun get_wr_offset_val iob = !(get_wr_offset_ref iob)

    fun get_wr_lwm (IOBuffer _ _ _ wr_lwm) = wr_lwm
end

structure IOBuffer =
struct
    (*  Initializes a buffer 
     *  @param initial_size `int`: initial size of an array
     *  @param wr_lwm `int`: low-memory watermark
     *)
    fun init (size : int) (wr_lwm : int) = 
        let
            val initial_array = Word8Array.array size (Word8.fromInt 0)
        in
            if wr_lwm > size then raise IOBufferLowWatermarkTooHigh size wr_lwm else ();

            IOBuffer (Ref initial_array) (Ref 0) (Ref 0) wr_lwm
        end

    (* Reset read/write pointers. Any unread content will be lost. *)
    fun reset iob = (
        (IOBufferType.get_rd_offset_ref iob) := 0;
        (IOBufferType.get_wr_offset_ref iob) := 0
    )

    (* Max number of bytes the buffer can hold *)
    fun max_size iob = Word8Array.length (IOBufferType.get_buffer_val iob)

    (* Number of bytes available to be read *)
    fun size iob = 
        (IOBufferType.get_wr_offset_val iob) - (IOBufferType.get_rd_offset_val iob)

    (* Number of bytes available to be written *)
    fun capacity iob = 
        max_size iob - (IOBufferType.get_wr_offset_val iob)

    (* Returns low-memory watermark indicating when the buffer should be
     * automatically crunched by the read() call. 
     *)
    fun wr_lwm iob = IOBufferType.get_wr_lwm iob

    (* Returns true if there's no data in the buffer *)
    fun empty iob = 
        size iob = 0

    (* Returns true when the write offset
     * passed the point of wr_lwm bytes from the end of the buffer. 
     *)
    fun is_low_space iob = 
        wr_lwm iob > capacity iob

    (* Ensure there's enough total space in the buffer to hold \a n bytes. *)
    fun reserve iob (n : int) =
        if n <= capacity iob 
        then ()
        else
            let
                val old_rd_offset = IOBufferType.get_rd_offset_val iob
                val dirty_size = size iob
                val old_buffer = IOBufferType.get_buffer_val iob

                val new_size = dirty_size + n
                val new_buffer = Word8Array.array new_size (Word8.fromInt 0)
            in
                Word8Array.copy old_buffer old_rd_offset dirty_size new_buffer 0;

                (IOBufferType.get_buffer_ref iob) := new_buffer;
                (IOBufferType.get_rd_offset_ref iob) := 0;
                (IOBufferType.get_wr_offset_ref iob) := dirty_size
            end

    (*  Read \a n bytes starting from the \offset bytes.
     *
     *  @param offset is the number of bytes to be skipped before 
     *      first byte to be read.
     *  @param n is the number of bytes to be read.
     *
     *  @returns n bytes of data that has been read from the rd_offset plus 
     *      \a offset as a string.
     *  @raises IOBufferNotEnoughData if there is not enough data
     *      in the buffer to read \a n bytes starting from \a offset.
     *)
    fun read_from iob (offset : int) (n : int) = 
        if n > (size iob) - offset
        then raise IOBufferNotEnoughData (size iob) offset n
        else
            let
                val rd_offset = IOBufferType.get_rd_offset_val iob
                val buffer = IOBufferType.get_buffer_val iob
            in
                Word8Array.substring buffer (rd_offset + offset) n
            end

    (*  Read \a n bytes from the buffer.
     *
     *  @param n is the number of bytes to read.
     *
     *  @returns n bytes of data that has been read from the rd_offset 
     *      as a string.
     *  @raises IOBufferNotEnoughData if there is not enough data 
     *      in the buffer to read \a n bytes.
     *)
    fun read iob (n : int) = read_from iob 0 n

    (*  Move any unread data to the beginning of the buffer.
     *  This function is similar to reset(), however, no unread
     *  data gets lost. 
     *)
    fun crunch iob =
        if (IOBufferType.get_rd_offset_val iob) = 0
        then ()
        else
            let
                val old_rd_offset = IOBufferType.get_rd_offset_val iob
                val size = size iob
                val buffer = IOBufferType.get_buffer_val iob
            in
                Word8Array.copy buffer old_rd_offset size buffer 0;

                (IOBufferType.get_rd_offset_ref iob) := 0;
                (IOBufferType.get_wr_offset_ref iob) := size
            end

    (*  Consume \a n bytes from the buffer (increment the rd_offset() by \a n).
     *
     *  @param n is the number of bytes to consume.
     *
     *  @raises IOBufferNotEnoughData if there is not enough data 
     *      in the buffer to consume \a n bytes.
     *)
    fun consume iob (n : int) =
        if n > size iob
        then raise IOBufferNotEnoughData (size iob) 0 n
        else
            let
                val rd_offset = IOBufferType.get_rd_offset_val iob
            in
                (IOBufferType.get_rd_offset_ref iob) := rd_offset + n
            end

    (*  Do the same action as consume(n), and call crunch() function
     *  when the buffer's capacity gets less than the wr_lwm() value.

     *  @param n is the number of bytes to consume.
     *  @raises IOBufferNotEnoughData if there is not enough data 
     *      in the buffer to consume \a n bytes.
     *)
    fun consume_and_crunch iob (n : int) = (
        consume iob n;
        if 
            is_low_space iob
        then 
            crunch iob
        else 
            ()
    )

    (*  Write data to a buffer from a given source \a src *)
    fun write iob (src : byte_array) =
        let
            val src_len = Word8Array.length src

            val _ = reserve iob src_len

            val buffer = IOBufferType.get_buffer_val iob
            val wr_offset = IOBufferType.get_wr_offset_val iob
        in
            Word8Array.copy src 0 src_len buffer wr_offset;
            
            (IOBufferType.get_wr_offset_ref iob) := wr_offset + src_len
        end
end

val _ = 
    Exception.add_exn_name_printer 
        (fn e => 
            case e of 
                IOBufferLowWatermarkTooHigh _ _ => "IOBufferLowWatermarkTooHigh"
              | IOBufferNotEnoughData _ _ _ => "IOBufferNotEnoughData"
              | _ => raise Exception.Unknown
        )
val _ = 
    Exception.add_exn_message_printer
        (fn e =>
            case e of
                IOBufferLowWatermarkTooHigh v1 v2 => 
                    "IOBufferLowWatermarkTooHigh" ^ " " ^ Int.toString v1 ^ " " ^ Int.toString v2
              | IOBufferNotEnoughData v1 v2 v3 => 
                    "IOBufferNotEnoughData" ^ " " ^ Int.toString v1 ^ " " ^ Int.toString v2 ^ " " ^ Int.toString v3
              | _ => raise Exception.Unknown
        )
