(**
 *  Is raised when FFI call returns an erroneous status code.
 *)
exception FFIFailure

(**
 *  Is raised when called by FFI syscall is interrupted by a signal.
 *)
exception FFIEintr

(**
 *  Is raised when called by FFI syscall would block 
 *  on nonblocking descriptor.
 *)
exception FFIEagain

structure FFICodes =
struct
    val success = 0
    val failure = 1
    val eintr = 2
    val eagain = 3
end

structure FFIHelper =
struct
    fun validate_status (outbuf : byte_array) =
        let
            val status = Word8.toInt (Word8Array.sub outbuf 0)
        in
            if status = FFICodes.failure
            then raise FFIFailure
            else if status = FFICodes.eintr
            then raise FFIEintr
            else if status = FFICodes.eagain
            then raise FFIEagain
            else ()
        end
end

local
    fun exn_printer e =
        case e of
            FFIFailure => "FFIFailure"
          | FFIEintr => "FFIEintr"
          | FFIEagain => "FFIEagain"
          | _ => raise Exception.Unknown
in
    val _ = Exception.add_exn_name_printer exn_printer
    val _ = Exception.add_exn_message_printer exn_printer
end
