(**
 *  Structure defines different kinds of messages to be logged and 
 *  describes their relations. In a logger user decides beginning from 
 *  what types they want messages to be really logged using defined levels.
 *)
structure LoggerLevel =
struct
    datatype level = Trace | Debug | Info | Warn | Error | Critical | Off

    (* Get the integer representation of the level enum. *)
    fun to_int (lvl : level) =
        case lvl of
            Trace => 0
          | Debug => 1
          | Info => 2
          | Warn => 3
          | Error => 4
          | Critical => 5
          | Off => 6
    
    (* Get the string representation of the level *)
    fun to_string (lvl : level) =
        case lvl of
            Trace => "Trace"
          | Debug => "Debug"
          | Info => "Info"
          | Warn => "Warn"
          | Error => "Error"
          | Critical => "Critical"
          | Off => "Off"

    
    (* Functions below define relations between defferent levels *)

    fun greater (a : level) (b : level) =
        Int.> (to_int a) (to_int b)

    fun less (a : level) (b : level) =
        Int.< (to_int a) (to_int b)

    fun geq (a : level) (b : level) =
        Int.>= (to_int a) (to_int b)

    fun leq (a : level) (b : level) =
        Int.<= (to_int a) (to_int b)

    fun equal (a : level) (b : level) =
        case (a, b) of
            (Trace, Trace) => True
          | (Debug, Debug) => True
          | (Info, Info) => True
          | (Warn, Warn) => True
          | (Error, Error) => True
          | (Critical, Critical) => True
          | (Off, Off) => True
          | _ => False
end
