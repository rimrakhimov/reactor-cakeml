signature IO_BUFFER =
sig
    type io_buffer

    val init : int -> int -> io_buffer
    val reset : io_buffer -> unit
    
    val size : io_buffer -> int
    val capacity : io_buffer -> int
    val empty : io_buffer -> bool

    val read : io_buffer -> int -> string
    val write : io_buffer -> byte_array -> unit

    val max_size : io_buffer -> int
    val wr_lwm : io_buffer -> int
    val is_low_space : io_buffer -> bool

    val reserve : io_buffer -> int -> unit
    
    val crunch : io_buffer -> unit
    val consume : io_buffer -> int -> unit
    val consume_and_crunch : io_buffer -> int -> unit
end