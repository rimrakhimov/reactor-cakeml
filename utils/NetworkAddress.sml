(* An alias that represents internet port as a separate type *)
type in_port = int

(*  The type represents internal representation of the ip (IPv4) address
 *  (in the future, support of IPv6 addresses may be added).
 *
 *  @param b0 `Word8.word`: the first byte of ip address
 *  @param b1 `Word8.word`: the second byte of ip address
 *  @param b2 `Word8.word`: the third byte of ip address
 *  @param b3 `Word8.word`: the fourth byte of ip address
 *)
datatype in_addr = InAddr Word8.word Word8.word Word8.word Word8.word

(*  The type represents an internet address consisting of 
 *  ip address and a port (for now only IPv4 addresses are supported).
 *
 *  @param addr `in_addr`: IPv4 address
 *  @param port `in_port`: port number
 *)
datatype sockaddr_in = SockAddrIn in_addr in_port

(*  Is raised when trying to get an ip address from invalid string
 *
 *  @param ip_str `string`: the string representing ip address to be transformed.
 *)
exception InAddrInvalidAddress string

(* Structure defines functions to be used with `in_addr` type *)
structure InAddr =
struct
    (*  Initializes an ip address parsing it from the string.
     *  For now only IPv4 addresses are supported in the format
     *  'a.b.c.d', where a, b, c, and d are some integers in 0.255 range.
     *)
    fun from_string (addr_s : string) = 
        let
            fun string_to_word8 (token : string) =
                case Int.fromString token of
                    (* Token cannot be parsed as an integer *)
                    None => raise InAddrInvalidAddress addr_s
                  | Some value => (
                        if value >= 0 andalso value <= 255
                        then Word8.fromInt value
                        else raise InAddrInvalidAddress addr_s
                    )

            val tokens = String.tokens (fn c => c = #".") addr_s
            
            val in_addr_size = 4
        in 
            if
                List.length tokens = in_addr_size
            then
                let
                    val [t0, t1, t2, t3] = tokens
                    val b0 = string_to_word8 t0
                    val b1 = string_to_word8 t1
                    val b2 = string_to_word8 t2
                    val b3 = string_to_word8 t3
                in
                    InAddr b0 b1 b2 b3
                end
            else
                (* IPv4 should have a form of `a.b.c.d`  *)
                raise InAddrInvalidAddress addr_s
        end

    local
        (* An alias for `Word8.toInt` to simplify the resultant code. *)
        val to_int = Word8.toInt
    in
        fun to_string (InAddr b0 b1 b2 b3) =
            let
                fun word8_to_string w = Int.toString (to_int w)
            in
                word8_to_string b0 ^ "." ^ word8_to_string b1 ^ "." ^
                word8_to_string b2 ^ "." ^ word8_to_string b3
            end

        fun compare (InAddr a0 a1 a2 a3) (InAddr b0 b1 b2 b3) =
            case Int.compare (to_int a0) (to_int b0) of
                Equal => (
                    case Int.compare (to_int a1) (to_int b1) of
                        Equal => (
                            case Int.compare (to_int a2) (to_int b2) of
                                Equal => Int.compare (to_int a3) (to_int b3)
                            | r2 => r2
                        )
                    | r1 => r1
                )
            | r0 => r0
    end
end

val _ = 
    Exception.add_exn_name_printer 
        (fn e => 
            case e of 
                InAddrInvalidAddress _ => "InAddrInvalidAddress"
              | _ => raise Exception.Unknown
        )
val _ = 
    Exception.add_exn_message_printer
        (fn e =>
            case e of
                InAddrInvalidAddress v => "InAddrInvalidAddress" ^ " " ^ v
              | _ => raise Exception.Unknown
        )
