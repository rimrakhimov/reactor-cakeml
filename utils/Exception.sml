(**
 *  Structure defines a storage for exception printers and
 *  functions to get strings from exceptions
 *)
structure Exception =
struct
    type exn_printer = (exn -> string)

    (**  
     *  Exception to be raised by printer functions 
     *  if exception does is not handled by the printer
     *)
    exception UnknownException

    local
        fun exn_printer_base exn = 
            case exn of
              (*    Basis primitive exceptions  *)
                Bind => "Bind"
              | Chr => "Chr"
              | Div => "Div"
              | Subscript => "Subscript"
  
              (*    TextIO type exceptions  *)
              | TextIO.BadFileName => "TextIO.BadFileName"
              | TextIO.InvalidFD => "TextIO.InvalidFD"
              | TextIO.EndOfFile => "TextIO.EndOfFile"
              | TextIO.IllegalArgument => "TextIO.IllegalArgument"
            
              (*    Raise if exception is unknown    *)
              | _ => raise UnknownException
    in
        val (exn_name_printers : exn_printer list ref) = Ref [exn_printer_base]
        
        val (exn_message_printers : exn_printer list ref) = Ref [exn_printer_base]
    end

    fun add_exn_name_printer printer =
        exn_name_printers := (printer :: (!exn_name_printers))

    fun add_exn_message_printer printer =
        exn_message_printers := (printer :: (!exn_message_printers))

    local
        fun printer_search exn (printers_list : exn_printer list) =
                case printers_list of
                    [] => "Unknown exception"
                  | (printer::others) =>
                        printer exn handle UnknownException => printer_search exn others
    in
        fun exn_name exn = printer_search exn (!exn_name_printers)

        fun exn_message exn = printer_search exn (!exn_message_printers)
    end
        
end ;
