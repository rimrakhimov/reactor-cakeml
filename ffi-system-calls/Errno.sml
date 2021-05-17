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
            val inbuf = ByteArray.to_string (MarshallingHelp.n2w4 errno)
            val outbuf = Word8Array.array 1026 (Word8.fromInt 0)
        in
            #(errno_strerror) inbuf outbuf;
            Word8Array.substring outbuf 1 (Word8.toInt (Word8Array.sub outbuf 1))
        end
end
