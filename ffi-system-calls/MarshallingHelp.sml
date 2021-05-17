structure MarshallingHelp =
struct
    val w2n = Word8.toInt

    val n2w = Word8.fromInt

    fun w42n bytes n = 
        let
            val b3 = Word8Array.sub bytes n
            val b2 = Word8Array.sub bytes (n + 1)
            val b1 = Word8Array.sub bytes (n + 2)
            val b0 = Word8Array.sub bytes (n + 3)
        in
            256 * (256 * (256 * w2n b3 + w2n b2) + w2n b1) + w2n b0
        end

    fun n2w4 i =
        let
            val out = Word8Array.array 4 (n2w 0)

            val _ = Word8Array.update out 3 (n2w i)
            val i = i div 256
            val _ = Word8Array.update out 2 (n2w i)
            val i = i div 256
            val _ = Word8Array.update out 1 (n2w i)
            val i = i div 256
            val _ = Word8Array.update out 0 (n2w i)
        in
            out
        end
        
end

