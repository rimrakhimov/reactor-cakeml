(**
 *  Definition of a timestamp. 
 *)
type timestamp = int

(**  
 *  Structure that defines functions to work with
 *  time connected ffi calls.
 *)
structure Timestamp =
struct
    (**
     *  A function to get current timestamp.
     *
     *  @returns `int` timestamp in nanoseconds.
     *
     *  @raises `FFIFailure` if `clock_gettime` syscall fails.
     *)
    fun current () =
        let
            val outbuf = ByteArray.empty 9 (* First byte to save error status. 8 others to save the timestamp *)
        in
                #(get_timestamp) "" outbuf;
                FFIHelper.validate_status outbuf;
                MarshallingHelp.w82n outbuf 1
        end
end
