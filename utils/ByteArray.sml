(**
 *  Structure defines some helper functions to work
 *  with byte_array.
 *)
structure ByteArray = 
struct
	fun empty size = Word8Array.array size (Word8.fromInt 0)

    fun from_string (src : string) =
		let
			val src_size = String.size src
			val result = Word8Array.array src_size (Word8.fromInt 0)
		in
			Word8Array.copyVec src 0 src_size result 0; 
            result
		end

    fun to_string (src : byte_array) =
		Word8Array.substring src 0 (Word8Array.length src)

    fun concat array1 array2 =
		let
			val length1 = Word8Array.length array1 
			val length2 = Word8Array.length array2
		  	val result = Word8Array.array (length1 + length2) (Word8.fromInt 0)
		in
		  	Word8Array.copy array1 0 length1 result 0;
			Word8Array.copy array2 0 length2 result length1;
			result
		end

	fun concat_all (arrays : byte_array list) = 
		List.foldr concat (empty 0) arrays

    fun compare array1 array2 =
        String.compare (to_string array1) (to_string array2)

    fun equal array1 array2 = 
        compare array1 array2 = Equal
end