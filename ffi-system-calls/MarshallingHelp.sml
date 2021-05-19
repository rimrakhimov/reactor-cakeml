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
    
    fun n2w8 i =
        let
            val out = Word8Array.array 8 (n2w 0)

            val _ = Word8Array.update out 7 (n2w i)
            val i = i div 256
            val _ = Word8Array.update out 6 (n2w i)
            val i = i div 256
            val _ = Word8Array.update out 5 (n2w i)
            val i = i div 256
            val _ = Word8Array.update out 4 (n2w i)
            val i = i div 256
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

    local
        fun w82n_internal b0 b1 b2 b3 b4 b5 b6 b7 =
            256 * (256 * (256 * (256 * (
                256 * (256 * (256 * w2n b7 + w2n b6) + w2n b5) + w2n b4
            ) + w2n b3) + w2n b2) + w2n b1) + w2n b0
    in
        fun w82n bytes n =
            let
                val b7 = Word8Array.sub bytes n
                val b6 = Word8Array.sub bytes (n + 1)
                val b5 = Word8Array.sub bytes (n + 2)
                val b4 = Word8Array.sub bytes (n + 3)
                val b3 = Word8Array.sub bytes (n + 4)
                val b2 = Word8Array.sub bytes (n + 5)
                val b1 = Word8Array.sub bytes (n + 6)
                val b0 = Word8Array.sub bytes (n + 7)
            in
                w82n_internal b0 b1 b2 b3 b4 b5 b6 b7  
            end

        fun w82n_little bytes n =
            let
                val b0 = Word8Array.sub bytes n
                val b1 = Word8Array.sub bytes (n + 1)
                val b2 = Word8Array.sub bytes (n + 2)
                val b3 = Word8Array.sub bytes (n + 3)
                val b4 = Word8Array.sub bytes (n + 4)
                val b5 = Word8Array.sub bytes (n + 5)
                val b6 = Word8Array.sub bytes (n + 6)
                val b7 = Word8Array.sub bytes (n + 7)
            in
                w82n_internal b0 b1 b2 b3 b4 b5 b6 b7
            end
    end

    (**
     *  Deserializes a byte array into the list of some elements.
     *
     *  @param f `byte_array -> int -> ('a * int)` - a function to deserialize
     *      a single element of the array. Returns an elemnts itself, and
     *      amount of bytes that has been wasted during deserialization.
     *  @param n `int`: number of elements to be deserialized.
     *  @param bytes `byte_array`: bytes to be deserialized.
     *  @param i `int`: index where bytes should be deserialized from.
     *)
    fun bytes_to_list f n bytes i =
        let
            fun internal k offset res =
                if 
                    k <= 0
                then 
                    res
                else
                    let
                        val (v, r) = f bytes offset
                    in
                        internal (k-1) (offset + r) (v::res)
                    end
        in
            List.rev (internal n i [])
        end
end

