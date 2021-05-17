structure Epoll =
struct
    fun create () =
        let
            val inbuf = ""
            val outbuf = Word8Array.array (1 + 4) (Word8.fromInt 0)

            val _ = #(epoll_create) inbuf outbuf
            val status = Word8.toInt (Word8Array.sub outbuf 0)
        in
            if status = FFICodes.success 
            then MarshallingHelp.w42n outbuf 1
            else raise FFIFailure
        end
end
