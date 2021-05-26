structure Timer =
struct
    (**
     *  Creates a new timer that can be manipulated via a file descriptor.
     *
     *  @returns `int`: a file descriptor of the created timer.
     *
     *  @raises `FFIFailure` if `timerfd_create` syscall fails.
     *)
    fun create () =
        let
            val inbuf = ByteArray.empty 0
            val outbuf = ByteArray.empty (1 + 4)
        in
            #(timer_create) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            MarshallingHelp.w42n outbuf 1
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
     *  @raises `FFIFailure` if `timerfd_settime` syscall fails.
     *)
    fun set_time (fd : int) (initial : int) (period : int) =
        let
            val inbuf = ByteArray.empty 20
            val _ = MarshallingHelp.n2w4 fd inbuf 0
            val _ = MarshallingHelp.n2w8 initial inbuf 4
            val _ = MarshallingHelp.n2w8 period inbuf 12

            val outbuf = ByteArray.empty 1
        in
            #(timer_set_time) (ByteArray.to_string inbuf) outbuf;
            FFIHelper.validate_status outbuf;
            ()
        end
end
