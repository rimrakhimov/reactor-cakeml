(**
 *  Is raised when FFI call returns an erroneous status code.
 *)
exception FFIFailure

structure FFICodes =
struct
    val success = 0
    val failure = 1
end

structure FFIHelper =
struct
    fun validate_status (outbuf : byte_array) =
        let
            val status = Word8.toInt (Word8Array.sub outbuf 0)
        in
            if status = FFICodes.failure
            then raise FFIFailure
            else ()
        end
end
