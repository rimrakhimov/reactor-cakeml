let
    val err_code = Word8Array.array 1 (Word8.fromInt 1)
in 
    if (!has_failed) then #(exit) "" err_code else ()
end;