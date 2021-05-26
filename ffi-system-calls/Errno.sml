structure Errno =
struct
    (**
     *  
     *)
    fun errno () =
        let
            val inbuf = ""
            val outbuf = Word8Array.array 4 (Word8.fromInt 0)
        in
            #(errno_errno) inbuf outbuf;
            MarshallingHelp.w42n outbuf 0
        end

    fun strerror (errno : int) =
        let
            val inbuf = ByteArray.empty 4
            val _ = MarshallingHelp.n2w4 errno inbuf 0

            val outbuf = Word8Array.array 1026 (Word8.fromInt 0)
        in
            #(errno_strerror) (ByteArray.to_string inbuf) outbuf;
            Word8Array.substring outbuf 1 (Word8.toInt (Word8Array.sub outbuf 1))
        end

    fun errno_strerror () = strerror (errno ())
end
