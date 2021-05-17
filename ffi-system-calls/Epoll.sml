structure Epoll =
struct
    fun create () =
        let
            val inbuf = ""
            val outbuf = Word8Array.array (1 + 4) (Word8.fromInt 0)
        in
            #(epoll_create) inbuf outbuf;
            FFIHelper.validate_status outbuf;
            MarshallingHelp.w42n outbuf 1
        end
end
