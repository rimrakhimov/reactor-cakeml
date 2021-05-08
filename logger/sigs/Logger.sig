signature LOGGER =
sig
    datatype level = Trace | Debug | Info | Warn | Error | Critical | Off
    type logger

    val create : TextIO.outstream * level -> logger
    val set_level : logger * level -> logger
    val level : logger -> level

    val trace : logger -> string -> unit
    val debug : logger -> string -> unit
    val info : logger -> string -> unit
    val warn : logger -> string -> unit
    val error : logger -> string -> unit
    val critical : logger -> string -> unit
end