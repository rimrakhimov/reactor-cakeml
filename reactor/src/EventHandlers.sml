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
 *  @param data `string`: data that was read from the descriptor.
 *
 *  @returns number of bytes that has been used from the incoming data.
 *)
type 'a read_handler = ('a -> int -> string -> int)

(**
 *  An event handler that is called when a socket is connected.
 *
 *  @param reactor `'b reactor`: current reactor state.
 *  @param fd `int`: file descriptor where event occured at.
 *)
type 'a connect_handler = ('a -> int -> unit)

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
type 'a accept_handler = ('a -> int -> int -> sockaddr_in -> unit)

(**
 *  An event handler that is called when a timer fires.
 *
 *  @param reactor `'b reactor`: current reactor state.
 *  @param fd `int`: file descriptor where event occured at.
 *)
type 'a timer_handler = ('a -> int -> unit)

(**
 *  An event handler that is called if any error occured 
 *  at the descriptor or during another event processing.
 *
 *  @param reactor `'b reactor`: current reactor state.
 *  @param fd `int`: file descriptor where error occured at.
 *)
type 'a err_handler = ('a -> int -> unit)
