(**  
 *  The type enumerates all possible types of file descriptors 
 *  and defines internal representations for each of the type.
 *  
 *  As in event handlers, `'a` is actually a parametric reactor type
 *  `'b reactor` which will be defined later, and where `'b` is defined
 *  by a client application and represents its internal state.
 *
 *  ReadWriteFileFdInfo is not defined as only pipes and FIFOs may be
 *  added into the polling mechanism. Both types of files can be opened
 *  either in read or write modes, but never in both.
 *)
datatype 'a fd_info =
    (*  ReadDataStreamInfo - constructor represents data streams (sockets)
     *  which can be read, but cannot be written:
     *      @param name `string`: client defined name of the descriptor.
     *      @param fd `int`: file descriptor.
     *      @param on_read `'a read_handler`: a concrete event handler 
     *          to be called back when new data is read.
     *      @param on_connect `'a connect_handler`: a concrete event handler
     *          to be called back when a connection is established.
     *      @param on_err `'a err_handler`: a concrete event handler
     *          to be called back when any error occurs on the descriptor.
     *      @param rd_buff `io_buffer`: a buffer where arrived data is stored 
     *          from the kernel buffer.
     *      @param is_connecting `bool`: a flag indicating whether the connection
     *          is in the proccess of establishment on the underlying socket.
     *      @param is_connected `bool`: a flag indicating whether the connection
     *          has been established on the underlying socket.
     *)
    ReadDataStreamFdInfo string int 
        ('a read_handler) ('a connect_handler) ('a err_handler) io_buffer bool bool

    (*  WriteDataStreamInfo - constructor represents data streams (sockets)
     *  which can be written, but cannot be read:
     *      @param name `string`: client defined name of the descriptor.
     *      @param fd `int`: file descriptor.
     *      @param on_connect `'a connect_handler`: a concrete event handler
     *          to be called back when a connection is established.
     *      @param on_err `'a err_handler`: a concrete event handler
     *          to be called back when any error occurs on the descriptor.
     *      @param wr_buff `io_buffer`: a buffer where not sent data is stored.
     *      @param is_connecting `bool`: a flag indicating whether the connection
     *          is in the proccess of establishment on the underlying socket.
     *      @param is_connected `bool`: a flag indicating whether the connection
     *          has been established on the underlying socket.
     *)
  | WriteDataStreamFdInfo string int
        ('a connect_handler) ('a err_handler) io_buffer bool bool

    (*  ReadWriteDataStreamInfo - constructor represents data streams (sockets)
     *  which can be both read and written:
     *      @param name `string`: client defined name of the descriptor.
     *      @param fd `int`: file descriptor.
     *      @param on_read `'a read_handler`: a concrete event handler 
     *          to be called back when new data is read.
     *      @param on_connect `'a connect_handler`: a concrete event handler
     *          to be called back when a connection is established.
     *      @param on_err `'a err_handler`: a concrete event handler
     *          to be called back when any error occurs on the descriptor.
     *      @param rd_buff `io_buffer`: a buffer where arrived data is stored 
     *          from the kernel buffer.
     *      @param wr_buff `io_buffer`: a buffer where not sent data is stored.
     *      @param is_connecting `bool`: a flag indicating whether the connection
     *          is in the proccess of establishment on the underlying socket.
     *      @param is_connected `bool`: a flag indicating whether the connection
     *          has been established on the underlying socket.
     *)
  | ReadWriteDataStreamFdInfo string int 
        ('a read_handler) ('a connect_handler) ('a err_handler) 
        io_buffer io_buffer bool bool

    (*  TimerFdInfo - constructor represents file descriptors that 
     *  corresponds to timers:
     *      @param name `string`: client defined name of the descriptor.
     *      @param fd `int`: file descriptor.
     *      @param on_timer `'a timer_handler`: a concrete event handler
     *          to be called back when the timer fires.
     *      @param on_err `'a err_handler`: a concrete event handler
     *          to be called back when any error occurs on the descriptor.
     *)
  | TimerFdInfo string int ('a timer_handler) ('a err_handler)

    (*  AcceptorFdInfo - constructor represents file descriptors that 
     *  corresponds to listening sockets:
     *      @param name `string`: client defined name of the descriptor.
     *      @param fd `int`: file descriptor.
     *      @param on_accept `'a accept_handler`: a concrete event handler
     *          to be called back when a new connection is accepted.
     *      @param on_err `'a err_handler`: a concrete event handler
     *          to be called back when any error occurs on the descriptor.
     *)
  | AcceptorFdInfo string int ('a accept_handler) ('a err_handler)

    (*  ReadFileFdInfo - constructor represents file descriptors that 
     *  corresponds to files opened in read only mode:
     *      @param name `string`: client defined name of the descriptor.
     *      @param fd `int`: file descriptor.
     *      @param on_read `'a read_handler`: a concrete event handler 
     *          to be called back when new data is read.
     *      @param on_err `'a err_handler`: a concrete event handler
     *          to be called back when any error occurs on the descriptor.
     *      @param rd_buff `io_buffer`: a buffer where read data is stored.
     *)
  | ReadFileFdInfo string int ('a read_handler) ('a err_handler) io_buffer

    (*  WriteFileFdInfo - constructor represents file descriptors that 
     *  corresponds to files opened in read only mode:
     *      @param name `string`: client defined name of the descriptor.
     *      @param fd `int`: file descriptor.
     *      @param on_err `'a err_handler`: a concrete event handler
     *          to be called back when any error occurs on the descriptor.
     *      @param wr_buff `io_buffer`: a buffer where not written data is stored.
     *)
  | WriteFileFdInfo string int ('a err_handler) io_buffer

(**
 *  Structure defines getters and setters for variables in fd_info representation.
 *)
structure FdInfoType =
struct
    (**
     *  Internal exception that is raised if the request cannot be
     *  executed on the corresponding type.
     *)
    exception InvalidType

    fun get_handler_type fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ _ _ _ _ _ _) => "ReadDataStream"
          | (WriteDataStreamFdInfo _ _ _ _ _ _ _) => "WriteDataStream"
          | (ReadWriteDataStreamFdInfo _ _ _ _ _ _ _ _ _) => "ReadWriteDataStream"
          | (TimerFdInfo _ _ _ _) => "Timer"
          | (AcceptorFdInfo _ _ _ _) => "Acceptor"
          | (ReadFileFdInfo _ _ _ _ _) => "ReadFile"
          | (WriteFileFdInfo _ _ _ _) => "WriteFile"


    fun get_name fd_info =  
        case fd_info of
            (ReadDataStreamFdInfo name _ _ _ _ _ _ _) => name
          | (WriteDataStreamFdInfo name _ _ _ _ _ _) => name
          | (ReadWriteDataStreamFdInfo name _ _ _ _ _ _ _ _) => name
          | (TimerFdInfo name _ _ _) => name
          | (AcceptorFdInfo name _ _ _) => name
          | (ReadFileFdInfo name _ _ _ _) => name
          | (WriteFileFdInfo name _ _ _) => name

    fun get_fd fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ fd _ _ _ _ _ _) => fd
          | (WriteDataStreamFdInfo _ fd _ _ _ _ _) => fd
          | (ReadWriteDataStreamFdInfo _ fd _ _ _ _ _ _ _) => fd
          | (TimerFdInfo _ fd _ _) => fd
          | (AcceptorFdInfo _ fd _ _) => fd
          | (ReadFileFdInfo _ fd _ _ _) => fd
          | (WriteFileFdInfo _ fd _ _) => fd

    fun get_err_handler fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ _ _ on_err _ _ _) => on_err
          | (WriteDataStreamFdInfo _ _ _ on_err _ _ _) => on_err
          | (ReadWriteDataStreamFdInfo _ _ _ _ on_err _ _ _ _) => on_err
          | (TimerFdInfo _ _ _ on_err) => on_err
          | (AcceptorFdInfo _ _ _ on_err) => on_err
          | (ReadFileFdInfo _ _ _ on_err _) => on_err
          | (WriteFileFdInfo _ _ on_err _) => on_err

    fun get_read_handler fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ on_read _ _ _ _ _) => on_read
          | (ReadWriteDataStreamFdInfo _ _ on_read _ _ _ _ _ _) => on_read
          | (ReadFileFdInfo _ _ on_read _ _) => on_read
          | _ => raise InvalidType

    fun get_connect_handler fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ _ on_connect _ _ _ _) => on_connect
          | (WriteDataStreamFdInfo _ _ on_connect _ _ _ _) => on_connect
          | (ReadWriteDataStreamFdInfo _ _ _ on_connect _ _ _ _ _) => on_connect
          | _ => raise InvalidType

    fun get_timer_handler fd_info =
        case fd_info of
            (TimerFdInfo _ _ on_timer _) => on_timer
          | _ => raise InvalidType

    fun get_accept_handler fd_info =
        case fd_info of
            (AcceptorFdInfo _ _ on_accept _) => on_accept
          | _ => raise InvalidType

    fun get_rd_buff fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ _ _ _ rd_buff _ _) => rd_buff
          | (ReadWriteDataStreamFdInfo _ _ _ _ _ rd_buff _ _ _) => rd_buff
          | (ReadFileFdInfo _ _ _ _ rd_buff) => rd_buff
          | _ => raise InvalidType

    fun get_wr_buff fd_info =
        case fd_info of
            (WriteDataStreamFdInfo _ _ _ _ wr_buff _ _) => wr_buff
          | (ReadWriteDataStreamFdInfo _ _ _ _ _ _ wr_buff _ _) => wr_buff
          | (WriteFileFdInfo _ _ _ wr_buff) => wr_buff
          | _ => raise InvalidType

    fun get_is_connecting_status fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ _ _ _ _ is_connecting _) => is_connecting
          | (WriteDataStreamFdInfo _ _ _ _ _ is_connecting _) => is_connecting
          | (ReadWriteDataStreamFdInfo _ _ _ _ _ _ _ is_connecting _) => is_connecting
          | _ => raise InvalidType
    fun set_is_connecting_status fd_info is_connecting =
        case fd_info of
            (ReadDataStreamFdInfo name fd on_read on_connect on_err rd_buff _ is_connected) => 
                (ReadDataStreamFdInfo name fd on_read on_connect on_err rd_buff is_connecting is_connected)
          | (WriteDataStreamFdInfo name fd on_connect on_err wr_buff _ is_connected) =>
                (WriteDataStreamFdInfo name fd on_connect on_err wr_buff is_connecting is_connected)
          | (ReadWriteDataStreamFdInfo name fd on_read on_connect on_err rd_buff wr_buff _ is_connected) => 
                (ReadWriteDataStreamFdInfo name fd on_read on_connect on_err rd_buff wr_buff is_connecting is_connected)
          | _ => raise InvalidType

    fun get_is_connected_status fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ _ _ _ _ _ is_connected) => is_connected
          | (WriteDataStreamFdInfo _ _ _ _ _ _ is_connected) => is_connected
          | (ReadWriteDataStreamFdInfo _ _ _ _ _ _ _ _ is_connected) => is_connected
          | _ => raise InvalidType
    fun set_is_connected_status fd_info is_connected =
        case fd_info of
            (ReadDataStreamFdInfo name fd on_read on_connect on_err rd_buff is_connecting _) => 
                (ReadDataStreamFdInfo name fd on_read on_connect on_err rd_buff is_connecting is_connected)
          | (WriteDataStreamFdInfo name fd on_connect on_err wr_buff is_connecting _) =>
                (WriteDataStreamFdInfo name fd on_connect on_err wr_buff is_connecting is_connected)
          | (ReadWriteDataStreamFdInfo name fd on_read on_connect on_err rd_buff wr_buff is_connecting _) => 
                (ReadWriteDataStreamFdInfo name fd on_read on_connect on_err rd_buff wr_buff is_connecting is_connected)
          | _ => raise InvalidType

    fun is_data_stream fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ _ _ _ _ _ _) => True
          | (WriteDataStreamFdInfo _ _ _ _ _ _ _) => True
          | (ReadWriteDataStreamFdInfo _ _ _ _ _ _ _ _ _) => True
          | _ => False
    
    fun is_read_data_stream fd_info =
        case fd_info of
            (ReadDataStreamFdInfo _ _ _ _ _ _ _ _) => True
          | _ => False
    
    fun is_write_data_stream fd_info =
        case fd_info of
            (WriteDataStreamFdInfo _ _ _ _ _ _ _) => True
          | _ => False
    
    fun is_read_write_data_stream fd_info =
        case fd_info of
            (ReadWriteDataStreamFdInfo _ _ _ _ _ _ _ _ _) => True
          | _ => False

    fun is_timer fd_info = 
        case fd_info of
            (TimerFdInfo _ _ _ _) => True
          | _ => False
    
    fun is_acceptor fd_info =
        case fd_info of
            (AcceptorFdInfo _ _ _ _) => True
          | _ => False
    
    fun is_file fd_info =
        case fd_info of
            (ReadFileFdInfo name _ _ _ _) => True
          | (WriteFileFdInfo name _ _ _) => True
          | _ => False
    
    fun is_read_file fd_info =
        case fd_info of
            (ReadFileFdInfo name _ _ _ _) => True
          | _ => False

    fun is_write_file fd_info =
        case fd_info of
            (WriteFileFdInfo name _ _ _) => True
          | _ => False
end