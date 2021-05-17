(**
 *  Is raised when FFI call returns an erroneous status code.
 *)
exception FFIFailure

structure FFICodes =
struct
    val success = 0
    val failure = 1
end
