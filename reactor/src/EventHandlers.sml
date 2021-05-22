datatype 'a reactor_function_request = 
    AddTimer string int int
        ('a timer_handler)
        ('a err_handler)
        ('a -> int -> 'a * 'a reactor_function_request option)
        ('a -> int -> 'a * 'a reactor_function_request option)
  | SetTimer int int int
        ('a -> 'a * 'a reactor_function_request option)
        ('a -> int -> 'a * 'a reactor_function_request option)
  | AddReadFile string int
        ('a read_handler) ('a err_handler) int int
        ('a -> 'a * 'a reactor_function_request option)
        ('a -> int -> 'a * 'a reactor_function_request option)
  | AddWriteFile string int
        ('a err_handler) int int
        ('a -> 'a * 'a reactor_function_request option)
        ('a -> int -> 'a * 'a reactor_function_request option)
  | ExitRun

(** 
 *  The file defines event handlers: function signatures which callback functions
 *  provided by client applications should correspond to. 
 *  
 *  `'a` is actually a parametric reactor type `('b reactor)` which will be defined later 
 *)

(**
 *  An event handler that is called when any readability event
 *  occurs at socket or file.
 *
 *  @param reactor `'b reactor`: current reactor state.
 *  @param fd `int`: file descriptor where event occured at.
 *  @param data `io_buffer`: a buffer with the data read from the descriptor.
 *
 *  @returns number of bytes that has been used from the incoming data.
 *)
and 'a read_handler = ReadHandler ('a -> int -> io_buffer -> 'a * int * 'a reactor_function_request option)

(**
 *  An event handler that is called when a socket is connected.
 *
 *  @param reactor `'b reactor`: current reactor state.
 *  @param fd `int`: file descriptor where event occured at.
 *)
and 'a connect_handler = ConnectHandler ('a -> int -> 'a * 'a reactor_function_request option)

(**
 *  An event handler that is called when a listening socket 
 *  accepts a new connection.
 *
 *  @param reactor `'b reactor`: current reactor state.
 *  @param acceptor_fd `int`: file descriptor where event occured at.
 *  @param client_fd `int`: file descriptor which was associated with new connection.
 *  @param clint_addr `sockaddr_in`: an address of the connected client 
 *      (IPv4 address and port number).
 *)
and 'a accept_handler = AcceptHandler ('a -> int -> int -> sockaddr_in -> 'a * 'a reactor_function_request option)

(**
 *  An event handler that is called when a timer fires.
 *
 *  @param reactor `'b reactor`: current reactor state.
 *  @param fd `int`: file descriptor where event occured at.
 *  @param count `int`: number of expirations occurred.
 *)
and 'a timer_handler = TimerHandler ('a -> int -> int -> 'a * 'a reactor_function_request option)

(**
 *  An event handler that is called if any error occured 
 *  at the descriptor or during another event processing.
 *
 *  @param reactor `'b reactor`: current reactor state.
 *  @param fd `int`: file descriptor where error occured at.
 *)
and 'a err_handler = ErrHandler ('a -> int -> 'a * 'a reactor_function_request option)
