(*  The type emumerates operations that can be performed
 *  for the target file descriptor as a part of control interface
 *  for an epoll file descriptor. 
 *)
datatype epoll_op_code = EpollCtlAdd | EpollCtlMod | EpollCtlDel

(**
 *  Structure defines some helper functions to process
 *  epoll_op_code datatype.
 *)
structure EpollOpCode =
struct
    (* Number of bytes required to encode an op_code. *)
    val size = 1

    (* Returns an integer corresponding to the op_code. *)
    fun to_int code =
        case code of
            EpollCtlAdd => 0
          | EpollCtlMod => 1
          | EpollCtlDel => 2

    (* Returns a serialized op_code. *)
    fun to_bytes code = 
        Word8Array.array size (Word8.fromInt (to_int code))
end
