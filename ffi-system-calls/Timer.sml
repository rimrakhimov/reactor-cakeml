structure Timer =
struct
    (**
     *  Creates a new timer that can be manipulated via a file descriptor.
     *
     *  @returns `int`: a file descriptor of the created timer.
     *
     *  @raises FFIFailure if `timerfd_create` syscall fails.
     *)
    fun create () =
        let
            val inbuf = ByteArray.empty 0
            val outbuf = ByteArray.empty (1 + 4)

            val _ = #(timer_create) (ByteArray.to_string inbuf) outbuf
            val status = Word8.toInt (Word8Array.sub outbuf 0)
        in
            if status = FFICodes.success 
            then MarshallingHelp.w42n outbuf 1
            else raise FFIFailure
        end
    
    (**
     *  Arms (starts) or disarms (stops) the timer
     *  referred to by the file descriptor \a fd.
     *
     *  @param fd `int`: a file descriptor that refers to the timer.
     *  @param initial `int`: a period of time in microseconds
     *      that the timer initially fires after.
     *  @param period `int`: a period of time in microseconds
     *      that the timer fires periodically after the first expiration.
     *
     *  @raises FFIFailure if `timerfd_settime` syscall fails.
     *)
    fun set_time (fd : int) (initial : int) (period : int) =
        let
            val fd_bytes = MarshallingHelp.n2w4 fd
            val initial_bytes = MarshallingHelp.n2w8 initial
            val period_bytes = MarshallingHelp.n2w8 period

            val inbuf = ByteArray.concat_all [fd_bytes, initial_bytes, period_bytes]
            val outbuf = ByteArray.empty 1

            val _ = #(timer_set_time) (ByteArray.to_string inbuf) outbuf
            val status = Word8.toInt (Word8Array.sub outbuf 0)
        in
            if status <> FFICodes.success
            then raise FFIFailure
            else ()
        end
end
