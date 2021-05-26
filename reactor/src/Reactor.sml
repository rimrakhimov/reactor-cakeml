(**
 *  The type defines internal representation of the reactor.
 *
 *  @param state_ref `'a ref`: a parametric value that keeps a reference
 *      to a client application state.
 *  @param logger `logger`.
 *  @param poll_fd `int`: the file descriptor corresponding to the epoll mechanism.
 *  @param fds_ref `(int, ('a reactor) fd_info) map ref`: a reference to a map from 
 *      file descriptors to corresponding fd_info structures. Reference is used
 *      in order to keep the reactor consistent with epoll mechanism.
 *  @param is_closed_ref `bool ref`: indicates whether the reactor has been closed. If is true,
 *      then the reactor was fully cleared and cannot be further operated.
 *)
datatype 'a reactor = Reactor ('a ref) logger int ((int, 'a fd_info) map ref) (bool ref)

(**
 *  Is raised to stop the reactor and exit from the run function.
 *  There is currently no other way of exiting from the "Run" infinite loop
 *  but to throw the following exception; so this is NOT actually an error condition!
 *)
exception ReactorExitRun

(**
 *  Is raised if any FFI call returns an error.
 *
 *  @param errno `int`: error number set by syscall. 
 *)
exception ReactorSystemError int

(**
 *  Is raised if a client application call some reactor 
 *  function with improper parameters.
 *)
exception ReactorBadArgumentError

(**
 *  Structure defines getters and setters for variables in reactor representation.
 *)
structure ReactorType =
struct
    fun get_state (Reactor state_ref _ _ _ _) = (!state_ref)
    fun get_logger (Reactor _ logger _ _ _) = logger
    fun get_epoll_fd (Reactor _ _ epoll_fd _ _) = epoll_fd
    fun get_fds (Reactor _ _ _ fds_ref _) = (!fds_ref)
    fun get_is_closed (Reactor _ _ _ _ is_closed_ref) = (!is_closed_ref)

    fun set_state (Reactor state_ref _ _ _ _) state = 
        state_ref := state
    fun set_logger (Reactor state_ref _ epoll_fd fds_ref is_closed_ref) logger = 
        Reactor state_ref logger epoll_fd fds_ref is_closed_ref
    fun set_epoll_fd (Reactor state_ref logger _ fds_ref is_closed_ref) epoll_fd =
        Reactor state_ref logger epoll_fd fds_ref is_closed_ref
    fun set_fds (Reactor _ _ _ fds_ref _) fds =
        fds_ref := fds
    fun set_is_closed (Reactor _ _ _ _ is_closed_ref) is_closed =
        is_closed_ref := is_closed

    (* Returns fd info if specified descriptor exists in the reactor. *)
    fun get_fd_info_opt reactor fd =
        Map.lookup (get_fds reactor) fd

    (* Returns whether specified descriptor exists in the reactor. *)
    fun has_fd_info reactor fd =
        Option.isSome (get_fd_info_opt reactor fd)

    (* Adds a new fd_info into the reactor or updates the existing one. *)
    fun add_fd_info reactor fd_info =
        let
            val fd = FdInfoType.get_fd fd_info
        in
            set_fds reactor (Map.insert (get_fds reactor) fd fd_info)
        end

    (* Removes fd_info from the reactor internal structure. *)
    fun remove_fd_info reactor fd =
        set_fds reactor (Map.delete (get_fds reactor) fd)
end

structure ReactorPrivate =
struct
    fun raise_reactor_system_error (before_f : int -> unit) (errno_opt : int option) =
        let
            val errno = case errno_opt of Some e => e | None => (Errno.errno ())
        in
            before_f errno;
            raise ReactorSystemError errno
        end

    (* Initializes the reactor. *)
    fun init state logger epoll_fd =
        Reactor (Ref state) logger epoll_fd (Ref (Map.empty Int.compare)) (Ref False)

    (* Returns whether an events returned by epoll include
     * any event corresponding to error. *)
    fun is_error (events_mask : epoll_events_mask) =
        EpollEventsMask.check events_mask Epollerr orelse 
        EpollEventsMask.check events_mask Epollhup orelse
        EpollEventsMask.check events_mask Epollrdhup

    (* Returns whether an events returned by epoll include readability event. *)
    fun is_readable (events_mask : epoll_events_mask) =
        EpollEventsMask.check events_mask Epollin

    (* Returns whether an events returned by epoll include writability event. *)
    fun is_writable (events_mask : epoll_events_mask) =
        EpollEventsMask.check events_mask Epollout

    fun fd_info_is_writable fd_info = (
        FdInfoType.is_write_data_stream fd_info
            orelse 
        FdInfoType.is_read_write_data_stream fd_info
            orelse
        FdInfoType.is_write_file fd_info
    )

    fun should_fd_info_be_closed fd_info =
        not (
            FdInfoType.is_read_file fd_info orelse FdInfoType.is_write_file fd_info
        )

    (*
     *  Closes a specified file descriptor. If closing retunrs an error,
     *  a warning is logged, but the error is ignored. 
     *  
     *  @param logger `logger`: a logger that will be used to journal errors.
     *  @param a_where `string`: a prefix that indicates the public reactor function 
     *      that initiated the closing.
     *  @param fd `int`: a file descriptor that should be closed.
     *)
    fun close_fd logger (a_where : string) (fd : int) = (
        Fd.close fd;
        Logger.info
            logger
            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^ ". Closed.")
    ) handle FFIFailure => (
        Logger.warn
            logger
            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
             ". Closing finished with an error: " ^
             Int.toString (Errno.errno()))
    )

    (**
     *  Internal function that removes a descriptor from the epoll
     *  mechanism and remove corresponding fd_info from reactor's 
     *  internal structure.
     *
     *  Any errros are ignored here, and only logged with a 'warn' level of logging.
     *
     *  @param reactor `'a reactor`: a reactor the file descriptor should be
     *      removed from.
     *  @param a_where `string`: a prefix that indicates the public reactor function
     *      that initiated the removal.
     *  @param fd `int`: a fd that should be removed.
     *  @param to_close `bool`: if true, the descriptor is closed after removing.
     *)
    fun remove_from_epoll reactor (a_where : string) (fd : int) (to_close : bool) =
        let
            val logger = ReactorType.get_logger reactor
            val epoll_fd = ReactorType.get_epoll_fd reactor
        in
            (* Detach file descriptor from the epoll mechanism. Ignore any error here. *)
            (
                Epoll.ctl EpollCtlDel epoll_fd fd (EpollEventsMask.empty ());
                Logger.info
                    logger
                    ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^ ". Detached.")
            )
            handle FFIFailure => (
                Logger.warn
                    logger
                    ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                     ". Detaching failed with Error=" ^ Int.toString (Errno.errno ()) ^ ".")
            );

            (* Close the file descriptor. Ignore any error here. *)
            if to_close
            then close_fd logger a_where fd
            else ();

            (* Remove file descriptor and corresponding info from the internal map *)
            ReactorType.remove_fd_info reactor fd
        end


    (**
     *  Internal function that adds a file descriptor 
     *  into the reactor's polling mechanism using FFI call to epoll_ctl
     *  and adds the descriptor's info into reactor's internal structure.
     *
     *  @param reactor `'a reactor`: a reactor where a file descriptor should be added.
     *  @param a_where `string`: a prefix that indicates the public reactor function 
     *      that initiated the addition.
     *  @param fd_info `'a reactor fd_info`: an fd_info that corresponds to the
     *      descriptor to be added.
     *  @param events_mask `epoll_events_mask`: events the reactor should listen on
     *      the descriptor to.
     *  @param close_on_error `bool`: if True, the descriptor is closed in case of errors.
     *
     *  @raises `ReactorSystemError` if call to `epoll_ctl` fails. In that case the socket
     *      will not be closed, and the caller function is responsible for the exception
     *      handling and closing the socket if required.
     *)
    fun add_to_epoll reactor (a_where : string) fd_info (events_mask : epoll_events_mask) (close_on_error : bool) =
        let
            val logger = ReactorType.get_logger reactor
            val epoll_fd = ReactorType.get_epoll_fd reactor

            val fd = FdInfoType.get_fd fd_info
        in
            Epoll.ctl EpollCtlAdd epoll_fd fd events_mask
            handle FFIFailure => (
                raise_reactor_system_error (fn errno => (
                    Logger.error
                        logger
                        ("Reactor." ^ a_where ^ ": Adding FD=" ^ Int.toString fd ^ 
                         ", Name=" ^ FdInfoType.get_name fd_info ^ 
                         ", HType=" ^ FdInfoType.get_handler_type fd_info ^ 
                         ". Failed with Error=" ^ Int.toString errno ^ ".");
                        if close_on_error then close_fd logger a_where fd else ()
                )) None
            );
            Logger.info 
                logger 
                ("Reactor." ^ a_where ^ ": Added FD=" ^ Int.toString fd ^ 
                 ", Name=" ^ FdInfoType.get_name fd_info ^ 
                 ", HType=" ^ FdInfoType.get_handler_type fd_info ^ ".");
            
            ReactorType.add_fd_info reactor fd_info
        end
    
    (**
     *  Internal implementation of the public `clear` function.
     *
     *  Any errros are ignored here, and only logged with a 'warn' level of logging.
     *
     *  @param reactor `'a reactor`: a reactor that should be cleared.
     *)
    fun clear reactor =
        let
            fun remove_all_fds fds_list =
                case fds_list of
                    [] => ()
                  | (fd_info :: others) => (
                        remove_from_epoll 
                            reactor "clear" (FdInfoType.get_fd fd_info) 
                            (should_fd_info_be_closed fd_info);
                        remove_all_fds others
                    )
            val fds_list = List.map snd (Map.toAscList (ReactorType.get_fds reactor))

            val logger = ReactorType.get_logger reactor
            val epoll_fd = ReactorType.get_epoll_fd reactor
        in
            Logger.info logger "Reactor.clear: clearing started.";
            remove_all_fds fds_list;
            close_fd logger "clear" epoll_fd;
            ReactorType.set_is_closed reactor True;
            Logger.info logger "Reactor.clear: reactor cleared"
        end

    (**
     *  Validates that the creration of the buffer with specified
     *  size and lower watermark arguments is valid.
     *
     *  @param logger `logger`: a logger that should be used to journal errors.
     *  @param a_where `string`: a name o
     *)
    fun validate_buffer_args logger (a_where : string) (name : string) 
            (bufsz : int) (lwm : int) (zero_allowed : bool) = (
        (* Check the common preconditions. *)
        if (bufsz < 0) orelse (lwm < 0) orelse (lwm > bufsz)
        then (
            Logger.error
                logger
                ("Reactor." ^ a_where ^ ": Name=" ^ name ^
                ". Invalid BuffSz or LowWaterMark." ^
                " Buffsz=" ^ Int.toString bufsz ^
                ", LowWaterMark=" ^ Int.toString lwm ^ ".");
            raise ReactorBadArgumentError
        ) else ();

        (* If buffer size should be positive, check that condition too. *)
        if (not zero_allowed) andalso (bufsz = 0)
        then (
            Logger.error
                logger
                ("Reactor." ^ a_where ^ ": Name=" ^ name ^
                ". Buffer must not be 0-size.");
            raise ReactorBadArgumentError
        ) else ()
    )
end

structure ReactorRequest = 
struct
    (**
     *  Assigns the address to the socket referred to by the file descriptor.
     *)
    fun bind logger (a_where : string) (fd : int) (address : in_addr) (port : int) = (
        Socket.bind fd address port
        handle FFIFailure => (
            ReactorPrivate.raise_reactor_system_error (fn errno => 
                Logger.error
                    logger
                    ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                     ". Binding failed with Error=" ^ Int.toString errno ^ ".")
            ) None
        );
        Logger.info 
            logger 
            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
             ". Binded to Addr=\"" ^ InAddr.to_string address ^ "\"" ^ 
             ", Port=" ^ Int.toString port ^ ".")
    )

    (**
     *  Just a wrapper under the Socket.create () function,
     *  that logs occurring events.
     *)
    fun create_socket logger (a_where : string) (name : string) =
        let
            val fd = Socket.create ()
            handle FFIFailure => (
                ReactorPrivate.raise_reactor_system_error (fn errno => (
                    Logger.error
                        logger
                        ("Reactor." ^ a_where ^ ": create_socket() failed with Error=" ^
                         Int.toString errno ^ ". Name=" ^ name ^ ".")
                )) None
            )
        in
            Logger.info
                logger
                ("Reactor." ^ a_where ^": socket created at FD=" ^ Int.toString fd ^
                 ". Name=" ^ name ^ ".");
            fd
        end

    local
        (**
         *  Creates a new socket and set it up with all predefined
         *  parameters.
         *
         *  @param logger `logger`
         *  @param a_where `string`: a public function that called us.
         *  @param name `string`: name of the socket provided by the user.
         *      Is used while logging errors and other infos.
         *
         *  @returns `int`: a file descriptor refferring to created socket.
         *
         *  @raises `ReactorSystemError` if any of fFFI calls fails.
         *)
        fun create_and_setup_data_stream logger (a_where : string) (name : string) (address_opt : in_addr option) =
            let
                val fd = create_socket logger a_where name
            in
                case address_opt of
                    None => ()
                  | Some address => bind logger a_where fd address 0;
                
                (* VERY IMPORTANT: Disable the Nagle algorithm on this socket. *)
                Socket.set_tcp_nodelay fd True
                handle FFIFailure => (
                    ReactorPrivate.raise_reactor_system_error (fn errno => 
                        Logger.error
                            logger
                            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                             ". Could not set TCP_NODELAY with Error=" ^ 
                             Int.toString errno ^ ".")
                    ) None
                );

                (** 
                 *  Set the Keep-Alive mode on the socket, with 1-second intervals:
                 *  ----------------------------------------------------------------------- *
                 *  5 missed acks will indicated a disconnect. This is required in order to
                 *  detect stale connections as early as possible: HeartBeats could also be
                 *  used to that end, but with much longer latency (eg minutes). 
                 *)
                Socket.set_so_keepalive fd True
                handle FFIFailure => (
                    ReactorPrivate.raise_reactor_system_error (fn errno => 
                        Logger.error
                            logger
                            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                             ". Could not set SO_KEEPALIVE with Error=" ^
                             Int.toString errno ^ ".")
                    ) None
                );

                (* Detailed config of TCP_KEEPALIVE option. *)
                Socket.set_tcp_keepidle fd 1
                handle FFIFailure => (
                    ReactorPrivate.raise_reactor_system_error (fn errno => 
                        Logger.error
                            logger
                            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                             ". Could not set TCP_KEEPIDLE with Error=" ^
                             Int.toString errno ^ ".")
                    ) None
                );
                Socket.set_tcp_keepintvl fd 1
                handle FFIFailure => (
                    ReactorPrivate.raise_reactor_system_error (fn errno => 
                        Logger.error
                            logger
                            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                             ". Could not set TCP_KEEPINTVL with Error=" ^
                             Int.toString errno ^ ".")
                    ) None
                );
                Socket.set_tcp_keepcnt fd 5
                handle FFIFailure => (
                    ReactorPrivate.raise_reactor_system_error (fn errno => 
                        Logger.error
                            logger
                            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                             ". Could not set TCP_KEEPCNT with Error=" ^
                             Int.toString errno ^ ".")
                    ) None
                );
                fd
            end

        (**
         *  Internal function that creates events_mask and
         *  specified fd_info into the reactor.
         *)
        fun add_data_stream 
            reactor (a_where : string) fd_info (is_read : bool) (is_write : bool) =
        let 
            (* Create the events mask *)
            val events_mask = EpollEventsMask.from_list [Epollet, Epollrdhup]
            val events_mask = if is_read then EpollEventsMask.add events_mask Epollin else events_mask
            val events_mask = if is_write then EpollEventsMask.add events_mask Epollout else events_mask
        in
            ReactorPrivate.add_to_epoll reactor a_where fd_info events_mask True
        end
    in
        (*
         *  Creates a new read data stream with specified parameters and adds it into the reactor.
         *
         *  @param reactor `'a reactor`.
         *  @param name `string`: a name that will be referring to the created data stream.
         *  @param on_read `'a read_handler`: a callback function called when new
         *      data are read from the data stream.
         *  @param on_connect_opt `'a connect_handler option`: if provided, the function
         *      is called when the socket descriptor reffers to is connected to another peer.
         *  @param on_error `'a reactor err_handler`
         *  @param rd_bufsz `int`: a size of the buffer corresponding to the descriptor.
         *  @param rd_lwm `int`: lower watermark that the buffer should have.
         *  @param address_opt `in_addr option`: if provided, specifies an IPv4 address 
         *      of the interface the data stream should be binded to.
         *)
        fun add_read_data_stream 
                reactor name on_read on_connect_opt on_error rd_bufsz rd_lwm address_opt =
            let
                val logger = ReactorType.get_logger reactor
                val a_where = "add_read_data_stream"
                
                (* Validate arguments before continuing. May raise `ReactorBadArgumentError`
                 * if corresponding arguments are non-valid. *)
                val _ = ReactorPrivate.validate_buffer_args logger a_where name rd_bufsz rd_lwm False

                (* Create a descriptor and set it up. *)
                val fd = create_and_setup_data_stream logger a_where name address_opt

                (* In theory could raise `IOBufferLowWatermarkTooHigh` exception
                 * but would not, as we verified that watermark is not greater
                 * than the buffer size. *)
                val rd_buff = IOBuffer.init rd_bufsz rd_lwm
                val fd_info = 
                    ReadDataStreamFdInfo name fd on_read on_connect_opt on_error rd_buff False False
            in
                add_data_stream reactor a_where fd_info True False;
                fd
            end
        
        (*
         *  Creates a new write data stream with specified parameters and adds it into the reactor.
         *
         *  @param reactor `'a reactor`.
         *  @param name `string`: a name that will be referring to the created data stream.
         *  @param on_connect_opt `'a connect_handler option`: if provided, the function
         *      is called when the socket descriptor reffers to is connected to another peer.
         *  @param on_error `'a reactor err_handler`
         *  @param wr_bufsz `int`: a size of the buffer corresponding to the descriptor.
         *  @param wr_lwm `int`: lower watermark that the buffer should have.
         *  @param address_opt `in_addr option`: if provided, specifies an IPv4 address 
         *      of the interface the data stream should be binded to.
         *)
        fun add_write_data_stream 
                reactor name on_connect_opt on_error wr_bufsz wr_lwm address_opt =
            let
                val logger = ReactorType.get_logger reactor
                val a_where = "add_write_data_stream"
                
                (* Validate arguments before continuing. May raise `ReactorBadArgumentError`
                 * if corresponding arguments are non-valid. *)
                val _ = ReactorPrivate.validate_buffer_args logger a_where name wr_bufsz wr_lwm True

                (* Create a descriptor and set it up. *)
                val fd = create_and_setup_data_stream logger a_where name address_opt

                (* In theory could raise `IOBufferLowWatermarkTooHigh` exception
                 * but would not, as we verified that watermark is not greater
                 * than the buffer size. *)
                val wr_buff = IOBuffer.init wr_bufsz wr_lwm
                val fd_info = 
                    WriteDataStreamFdInfo name fd on_connect_opt on_error wr_buff False False
            in
                add_data_stream reactor a_where fd_info False True;
                fd
            end

        (*
         *  Creates a new read write data stream with specified parameters and adds it into the reactor.
         *
         *  @param reactor `'a reactor`.
         *  @param name `string`: a name that will be referring to the created data stream.
         *  @param on_read `'a read_handler`: a callback function called when new
         *      data are read from the data stream.
         *  @param on_connect_opt `'a connect_handler option`: if provided, the function
         *      is called when the socket descriptor reffers to is connected to another peer.
         *  @param on_error `'a reactor err_handler`
         *  @param rd_bufsz `int`: a size of the read buffer corresponding to the descriptor.
         *  @param rd_lwm `int`: lower watermark that the read buffer should have.
         *  @param wr_bufsz `int`: a size of the write buffer corresponding to the descriptor.
         *  @param wr_lwm `int`: lower watermark that the write buffer should have.
         *  @param address_opt `in_addr option`: if provided, specifies an IPv4 address 
         *      of the interface the data stream should be binded to.
         *)
        fun add_read_write_data_stream 
                reactor name on_read on_connect_opt on_error rd_bufsz rd_lwm wr_bufsz wr_lwm address_opt =
            let
                val logger = ReactorType.get_logger reactor
                val a_where = "add_read_write_data_stream"
                
                (* Validate arguments before continuing. May raise `ReactorBadArgumentError`
                 * if corresponding arguments are non-valid. *)
                val _ = ReactorPrivate.validate_buffer_args logger a_where name rd_bufsz rd_lwm False
                val _ = ReactorPrivate.validate_buffer_args logger a_where name wr_bufsz wr_lwm True

                (* Create a descriptor and set it up. *)
                val fd = create_and_setup_data_stream logger a_where name address_opt

                (* In theory could raise `IOBufferLowWatermarkTooHigh` exception
                 * but would not, as we verified that watermark is not greater
                 * than the buffer size. *)
                val rd_buff = IOBuffer.init rd_bufsz rd_lwm
                val wr_buff = IOBuffer.init wr_bufsz wr_lwm
                val fd_info = 
                    ReadWriteDataStreamFdInfo name fd on_read on_connect_opt on_error rd_buff wr_buff False False
            in
                add_data_stream reactor a_where fd_info True True;
                fd
            end
    end

    (**
     *  Initializes a connection on a socket with specified peer address.
     *
     *  @param reactor `'a reactor`
     *  @param fd `int`: a file descriptor referring to the socket.
     *  @param sockaddr `sockaddr_in`: an address of the peer to initialize
     *      connection to.
     *)
    fun connect reactor fd sockaddr =
        let
            val logger = ReactorType.get_logger reactor

            val fd_info = 
                case ReactorType.get_fd_info_opt reactor fd of
                    Some fd_info => fd_info
                  | None => (
                        Logger.error
                            logger
                            ("Reactor.connect: FD=" ^ Int.toString fd ^
                             ". The descriptor does not belong to the reactor.");
                        raise ReactorBadArgumentError
                  )

            val (SockAddrIn address port) = sockaddr
            (* Internal function that connects the socket, while ignoring
             * interruption (EINTR) errors. `FFIEagain` and `FFIFailure` 
             * exceptions are wrapped into Error type and returned. *)
            fun internal () = (
                Ok (Socket.connect fd address port)
                handle 
                    FFIEintr => internal ()
                  | FFIEagain => Error FFIEagain
                  | FFIFailure => Error FFIFailure

            )
        in
            (* Validate that fd_info indeed corresponds to a data stream. *)
            if not (
                FdInfoType.is_read_data_stream fd_info
                    orelse
                FdInfoType.is_write_data_stream fd_info
                    orelse
                FdInfoType.is_read_write_data_stream fd_info
            ) then (
                Logger.error
                    logger
                    ("Reactor.connect: FD=" ^ Int.toString fd ^
                     ". Corresponding fd_info is not a DataStream.");
                raise ReactorBadArgumentError
            ) else ();

            (* We cannot perform repeated connection attempts, thus, verify
             * that the connection is not establishing or has been established
             * on the socket. *)
            if (
                FdInfoType.get_is_connecting_status fd_info 
                    orelse 
                FdInfoType.get_is_connected_status fd_info
            ) then (
                Logger.error
                    logger
                    ("Reactor.connect: FD=" ^ Int.toString fd ^ 
                     ". Socket already connected, or connection is in progress.");
                raise ReactorBadArgumentError
            ) else ();

            (*  IMPORTANT: if the FDInfo refers to Read only data stream, it will 
             *  not have EPOLLOUT event specified. However, when connection terminates,
             *  exactly this event will be emitted on the socket. Thus, we have to
             *  add the EPOLLOUT event before trying to connect. On connection,
             *  the event should be deleted from the set of events, descriptor is
             *  waiting for.
             *) 
            if (FdInfoType.is_read_data_stream fd_info)
            then (
                let
                    val epoll_fd = ReactorType.get_epoll_fd reactor
                    val events_mask = EpollEventsMask.from_list [Epollet, Epollrdhup, Epollin, Epollout]
                in
                    Epoll.ctl EpollCtlMod epoll_fd fd events_mask
                    handle FFIFailure => (
                        ReactorPrivate.raise_reactor_system_error (fn errno => 
                            Logger.error
                                logger
                                ("Reactor.connect: FD=" ^ Int.toString fd ^
                                 ". Specifying EPOLLOUT event in the epoll for ReadDataStream" ^
                                 " descriptor failed with Error=" ^ Int.toString errno ^ ".")
                        ) None
                    )
                end
            ) else ();

            case internal () of
                Ok () => (
                    (* TCP Connect already succeeded (which is nearly impossible).
                     * In that case we store connected status inside the FDInfo.
                     * 
                     * Likewise, it seems that we should call on_connect callback
                     * function, but we do not do it for now. TODO. *)
                    Logger.info
                        logger
                        ("Reactor.connect: FD=" ^ Int.toString fd ^
                         ". TCP Connect Successful.");
                    ReactorType.add_fd_info reactor (FdInfoType.set_is_connected_status fd_info True)
                )
              | Error FFIEagain => (
                    (* It is a normal condition indicating that connection has
                     * not been established immideately. We just store that 
                     * information in the added FDInfo and return from the function. 
                     *)
                    Logger.info
                        logger
                        ("Reactor.connect: FD=" ^ Int.toString fd ^
                         ". TCP Connection is in progress.");
                    ReactorType.add_fd_info reactor (FdInfoType.set_is_connecting_status fd_info True)
                )
              | Error FFIFailure => (
                    ReactorPrivate.raise_reactor_system_error (fn errno => 
                        Logger.error
                            logger
                            ("Reactor.connect: FD=" ^ Int.toString fd ^ 
                             ". connect() failed with Error=" ^ Int.toString errno ^ ".")
                    ) None
                )
        end

    (**
     *  Creates a new acceptance socket and add it into the reactor.
     *
     *  @param reactor `'a reactor`.
     *  @param name `string`: a name that will be referring to the created acceptor.
     *  @param on_accept `'a accept_handler`: a callback function that should be
     *      called when new connection is accepted.
     *  @param on_error `'a err_handler`: a callback function that should be called
     *      if any error will be obtained when accepting new connections.
     *  @param sockaddr `sockaddr_in`: an IPv4 address and port number the 
     *      listening socket should be binded to.
     *  @param backlog `int`: the maximum length to which the queue of
     *      pending connections for the socket may grow.
     *
     *  @returns `int`: a file descriptor referring to created listening socket.
     *
     *  @raises `ReactorSystemError` if any applied syscall fails.
     *)
    fun add_acceptor reactor name on_accept on_error sockaddr backlog = 
        let
            val logger = ReactorType.get_logger reactor
            val a_where = "add_acceptor"

            val fd = create_socket logger a_where name
            val (SockAddrIn in_addr in_port) = sockaddr

            val fd_info = AcceptorFdInfo name fd on_accept on_error
            val events_mask = EpollEventsMask.from_list [Epollet, Epollin]
        in
            (* Allow to reuse local addresses before binding the socket. *)
            Socket.set_so_reuseaddr fd True
            handle FFIFailure => (
                ReactorPrivate.raise_reactor_system_error (fn errno => 
                    Logger.error
                        logger
                        ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                         ". Could not set SO_REUSEADDR with Error=" ^
                         Int.toString errno ^ ".")
                ) None
            );
            
            (* Bind the socket to the specified address *)
            bind logger a_where fd in_addr in_port;

            (* Make the socket listening. *)
            Socket.listen fd backlog
            handle FFIFailure => (
                ReactorPrivate.raise_reactor_system_error (fn errno => 
                    Logger.error
                        logger
                        ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                         ". listen() failed with Error=" ^
                         Int.toString errno ^ ".")
                ) None
            );
            Logger.info
                logger
                ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                 ". Start listening.");

            (* Add the created listening socket into the epoll mechanism. *)
            ReactorPrivate.add_to_epoll reactor a_where fd_info events_mask True;
            fd
        end

    (*
     *  Creates a new timer with specified parameters and adds it into the reactor.
     *
     *  @param reactor `'a reactor`.
     *  @param name `string`: a name that will be referring to the created timer.
     *  @param initial_mcsec `int`: a period of time in microseconds
     *      after which the timer will expire first time.
     *  @param period_mcsec `int`: a period of time in microseconds
     *      that timer periodically fires after the first expiration.
     *  @param on_timer `'a reactor timer_handler`
     *  @param on_error `'a reactor err_handler`
     *
     *  @returns `(int * 'a reactor)`: a timer fd created, and an updated reactor.
     *
     *  @raises `ReactorSystemError` if timer creation, timer setting, or addtion
     *      into the epoll mechanism return an error.
     *)
    fun add_timer reactor (name : string) (initial_mcsec : int) (period_mcsec : int)
            on_timer on_error =
        let
            val logger = ReactorType.get_logger reactor

            val fd = Timer.create ()
            handle FFIFailure => (
                ReactorPrivate.raise_reactor_system_error (fn errno => 
                    Logger.error
                        logger
                        ("Reactor.add_timer: create_timer() failed with Error=" ^
                        Int.toString errno ^ ". Name=" ^ name ^ ".")
                ) None
            )
            val _ = Logger.info logger 
                ("Reactor.add_timer: timer created at FD=" ^ Int.toString fd ^ ". Name=" ^ name ^ ".")

            val _ = Timer.set_time fd initial_mcsec period_mcsec
            handle FFIFailure => (
                ReactorPrivate.raise_reactor_system_error (fn errno => (
                    Logger.error
                        logger
                        ("Reactor.add_timer: set_timer() failed with Error=" ^
                         Int.toString errno ^ ". FD=" ^ Int.toString fd ^ ".");
                    ReactorPrivate.close_fd logger "add_timer" fd
                )) None
            )
            val _ = Logger.info logger
                ("Reactor.add_timer: FD=" ^ Int.toString fd ^ ". Timer was set.")

            (*  We are allowing the buffer to handle 31 expirations without crunching. 
             *  When a timer expires 31 times, there are only 8 bytes left to read the data,
             *  and, thus, we cannot detect EAGAIN condition while reading 32nd expiration,
             *  without crunching the buffer before.
             *)
            val buff = IOBuffer.init (8 * 32) 9
            val fd_info = TimerFdInfo name fd on_timer on_error buff
            val events_mask = EpollEventsMask.from_list [Epollet, Epollin]
        in
            ReactorPrivate.add_to_epoll reactor "add_timer" fd_info events_mask True;
            fd
        end

    (*
     *  Arms (starts) or disarms (stops) the timer referred to by 
     *  the file descriptor included into the reactor.
     *
     *  @param reactor `'a reactor`.
     *  @param fd `int`: a file descriptor refferring to the timer.
     *  @param initial_mcsec `int`: a period of time in microseconds
     *      after which the timer will expire the first time.
     *  @param period_mcsec `int`: a period of time in microseconds
     *      that timer periodically fires after the first expiration.
     *
     *  @raises `ReactorSystemError` if timer setting returns an error.
     *      Corresponding file descriptor is removed from the reactor
     *      and closed.
     *  @raises `ReactorBadArgumentError` if provided file descriptor does
     *      not belong to the reactor or is not a TimerFdInfo.
     *)
    fun set_timer reactor (fd : int) (initial_mcsec : int) (period_mcsec : int) =
        let
            val logger = ReactorType.get_logger reactor
            
            val fd_info = 
                case ReactorType.get_fd_info_opt reactor fd of
                    Some fd_info => fd_info
                  | None => (
                        Logger.error
                            logger
                            ("Reactor.set_timer: FD=" ^ Int.toString fd ^
                             ". The descriptor does not belong to the reactor.");
                        raise ReactorBadArgumentError
                  )
        in
            (* Validate that fd_info indeed belongs to a timer. *)
            if not (FdInfoType.is_timer fd_info)
            then (
                Logger.error
                    logger
                    ("Reactor.set_timer: FD=" ^ Int.toString fd ^
                     ". Corresponding fd_info is not a Timer.");
                raise ReactorBadArgumentError
            )
            else ();

            Timer.set_time fd initial_mcsec period_mcsec
            handle FFIFailure => (
                ReactorPrivate.raise_reactor_system_error (fn errno => (
                    Logger.error
                        logger
                        ("Reactor.set_timer: FD=" ^ Int.toString fd ^ ". set_timer() failed.")
                )) None
            );

            Logger.info 
                logger
                ("Reactor.set_timer: FD=" ^ Int.toString fd ^ ". Timer was set.")
        end

    local
        (**
         *  Internal implementation to add read or write files into the reactor.
         *
         *  @param reactor `'a reactor`: a reactor where files should be added into.
         *  @param name `string`: a name that will be referring to added file.
         *  @param fd `int`: a file descriptor that refers to added file.
         *  @param on_read_opt `'a reactor read_handler option`: an optional field
         *      with on read event handler. We use these argument to distinguish between
         *      `add_read_file` and `add_write_file` callers.
         *  @param on_error `'a reactor err_handler`
         *  @param bufsz `int`: a size of the buffer corresponding to the file.
         *  @param lwm `int`: lower watermark that the buffer should have.
         *
         *  @raises `ReactorBadArgumentError` if any of the arguments is invalid.
         *  @raises `ReactorSystemError` if syscalls to make file nonblocking,
         *      or add file into the epoll mechanism fails.
         *)
        fun add_file reactor (name : string) (fd : int) on_read_opt on_error
                (bufsz : int) (lwm : int) =
            let
                (* If there is an read handler, then we deal with a read file,
                 * otherwise we deal with write file. *)
                val is_read = Option.isSome on_read_opt
                val a_where = if is_read then "add_read_file" else "add_write_file"
                (* Buffer with zero size is not allowed for read files,
                 * as there would be no space to read data into. *)
                val zero_allowed = if is_read then False else True

                val logger = ReactorType.get_logger reactor
            in
                (* Validate arguments before continuing. May raise `ReactorBadArgumentError`
                 * if corresponding arguments are non-valid. *)
                ReactorPrivate.validate_buffer_args logger a_where name bufsz lwm zero_allowed;

                (* IMPORTANT: the descriptor may be created blocking. Thus, explicitly 
                 * set the descriptor into non-blocking mode, before using it in the reactor. *)
                Fd.set_blocking fd False
                handle FFIFailure => (
                    ReactorPrivate.raise_reactor_system_error (fn errno => (
                        Logger.error
                            logger
                            ("Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                             ". Could not set O_NONBLOCK mode. Failed with Error=" ^ 
                             Int.toString errno ^ ".")
                    )) None
                );

                let
                    (* In theory could raise `IOBufferLowWatermarkTooHigh` exception
                     * but would not, as we verified that watermark is not greater
                     * than the buffer size. *)
                    val buff = IOBuffer.init bufsz lwm
                    val fd_info = 
                        if is_read 
                        then ReadFileFdInfo name fd (Option.valOf on_read_opt) on_error buff
                        else WriteFileFdInfo name fd on_error buff

                    (* Create the events mask *)
                    val ev = if is_read then Epollin else Epollout
                    val events_mask = EpollEventsMask.from_list [Epollet, Epollrdhup, ev]
                in
                    (* Even if addition to the epoll mechanism failed, we would not close 
                     * the descriptor as it was not opened by us.
                     *)
                    ReactorPrivate.add_to_epoll reactor a_where fd_info events_mask False
                end
            end
    in
        fun add_read_file reactor (name : string) (fd : int) on_read on_error
                (rd_bufsz : int) (rd_lwm : int) =
            add_file reactor name fd (Some on_read) on_error rd_bufsz rd_lwm

        fun add_write_file reactor (name : string) (fd : int) on_error 
                (wr_bufsz : int) (wr_lwm : int) = 
            add_file reactor name fd None on_error wr_bufsz wr_lwm
    end

    fun write reactor fd data =
        let
            val logger = ReactorType.get_logger reactor

            val fd_info = 
                case ReactorType.get_fd_info_opt reactor fd of
                    Some fd_info => fd_info
                  | None => (
                        Logger.error
                            logger
                            ("Reactor.write: FD=" ^ Int.toString fd ^
                             ". The descriptor does not belong to the reactor.");
                        raise ReactorBadArgumentError
                  )

            (* Validate that fd_info is indeed writable. *)
            fun validate_fd_info () =
                if  
                    not (ReactorPrivate.fd_info_is_writable fd_info)
                then (
                    Logger.error
                        logger
                        ("Reactor.write: FD=" ^ Int.toString fd ^
                         ". Corresponding fd_info is not writable.");
                    raise ReactorBadArgumentError
                ) else if (
                    (* If the descriptor corresponds to a socket, we require
                     * that the socket MUST be connected prior to the "write". *)
                    (
                        FdInfoType.is_write_data_stream fd_info
                            orelse 
                        FdInfoType.is_read_write_data_stream fd_info
                    ) andalso not (FdInfoType.get_is_connected_status fd_info)
                ) then (
                    Logger.error
                        logger
                        ("Reactor.write: FD=" ^ Int.toString fd ^
                         ". Corresponding fd_info refers to an unconnected socket.");
                        raise ReactorBadArgumentError
                ) else ();
            val _ = validate_fd_info ()

            val wr_buff = FdInfoType.get_wr_buff fd_info
            val initial_buff_size = IOBuffer.size wr_buff
            val n = IO.write_until_eagain fd data wr_buff
                handle 
                    FFIFailure => (
                        ReactorPrivate.raise_reactor_system_error (fn errno => (
                            Logger.error
                                logger
                                ("Reactor.write: FD=" ^ Int.toString fd ^ ". write() failed.")
                        )) None
                    )
                  | IOBufferOverflow => (
                        Logger.error
                            logger
                            ("Reactor.write: FD=" ^ Int.toString fd ^ ".write() failed with Buffer Overflow");
                        raise IOBufferOverflow
                    )

            val written_from_buff = min n initial_buff_size
            val written_from_data = max 0 (n - initial_buff_size)
        in
            Logger.info
                logger
                ("Reactor.write: FD=" ^ Int.toString fd ^
                 ". Written from buffer n_buff=" ^ Int.toString written_from_buff ^ " bytes" ^
                 ", written from data n_data=" ^ Int.toString written_from_data ^ " bytes.")
        end

    (**
     *  Removes a descriptor from the reactor.
     *
     *  @param reactor `'a reactor`
     *  @param fd `int`: a descriptor that should be removed.
     *
     *  All exceptions are logged, but not propagates further.
     *)
    fun remove reactor (fd : int) =
        let
            val logger = ReactorType.get_logger reactor
            val fd_info_opt =  ReactorType.get_fd_info_opt reactor fd
        in
            case fd_info_opt of
                None => (
                    Logger.warn
                        logger
                        ("Reactor.remove: FD=" ^ Int.toString fd ^
                         ". The descriptor does not belong to the reactor.")
                )
              | Some fd_info => (
                    ReactorPrivate.remove_from_epoll
                        reactor "remove" fd
                        (ReactorPrivate.should_fd_info_be_closed fd_info)
              )
               
        end


    (**
     *  Stops the reactor execution. Should be used very carefully,
     *  as the execution of running loop is stopped, and all uprorcessed
     *  events on descriptors are lost.
     *
     *  @param reactor `'a reactor` a reactor which running loop should be stopped.
     *
     *  @raise `ReactorExitRun` that should be catched in the run loop. 
     *)
    fun exit_run reactor = 
        let
            val logger = ReactorType.get_logger reactor
        in
            Logger.info logger "Reactor.exit_run: Reactor is stopping.";
            raise ReactorExitRun
        end
end

structure ReactorInternal =
struct
    local
        fun process_error reactor error_callback errno =
            let
                val (new_state, new_request_opt) = error_callback (ReactorType.get_state reactor) errno
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_add_read_data_stream_request 
                reactor name on_read on_connect_opt on_error rd_bufsz rd_lwm in_addr_opt callback =
            let
                val fd = ReactorRequest.add_read_data_stream 
                    reactor name on_read on_connect_opt on_error rd_bufsz rd_lwm in_addr_opt
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor) fd
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end
        
        fun process_add_write_data_stream_request 
                reactor name on_connect_opt on_error wr_bufsz wr_lwm in_addr_opt callback =
            let
                val fd = ReactorRequest.add_write_data_stream 
                    reactor name on_connect_opt on_error wr_bufsz wr_lwm in_addr_opt
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor) fd
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_add_read_write_data_stream_request 
                reactor name on_read on_connect_opt on_error rd_bufsz rd_lwm wr_bufsz wr_lwm in_addr_opt callback =
            let
                val fd = ReactorRequest.add_read_write_data_stream 
                    reactor name on_read on_connect_opt on_error rd_bufsz rd_lwm wr_bufsz wr_lwm in_addr_opt
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor) fd
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_connect_request reactor fd sockaddr callback =
            let
                val _ = ReactorRequest.connect reactor fd sockaddr
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor)
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end


        fun process_add_acceptor_request reactor name on_accept on_error sockaddr backlog callback =
            let
                val fd = ReactorRequest.add_acceptor reactor name on_accept on_error sockaddr backlog
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor) fd
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_add_timer_request reactor name initial_mcsec period_mcsec on_timer on_err callback =
            let
                val fd = ReactorRequest.add_timer reactor name initial_mcsec period_mcsec on_timer on_err
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor) fd
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_set_timer_request reactor fd initial_mcsec period_mcsec callback =
            let
                val _ = ReactorRequest.set_timer reactor fd initial_mcsec period_mcsec
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor)
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_add_read_file_request reactor name fd on_read on_error rd_bufsz rd_lwm callback =
            let
                val _ = ReactorRequest.add_read_file reactor name fd on_read on_error rd_bufsz rd_lwm
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor)
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_add_write_file_request reactor name fd on_error rd_bufsz rd_lwm callback =
            let
                val _ = ReactorRequest.add_write_file reactor name fd on_error rd_bufsz rd_lwm
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor)
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        (**
         *  IMPORTANT: returning from Write request does not mean, that
         *  that data has been actually written neither into the sending wires,
         *  nor even into the kernel buffer. Some data may remain unsent and stored
         *  into the FDInfo buffer to be sent later by `DelayedSend`.
         *  Thus, the user should not remove the descriptor, as a callback.
         *
         *  The callback functionality is added for the case if more than 1 write
         *  operation should be performed in a row to several different descriptors.
         *)
        fun process_write_request reactor fd data callback =
            let
                val _ = ReactorRequest.write reactor fd data
                handle IOBufferOverflow => raise ReactorSystemError (~1)

                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor)
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_remove_request reactor fd callback =
            let
                val _ = ReactorRequest.remove reactor fd
                val (new_state, new_request_opt) = callback (ReactorType.get_state reactor)
            in
                ReactorType.set_state reactor new_state;
                (reactor, new_request_opt)
            end

        fun process_exit_run reactor = 
            ReactorRequest.exit_run reactor

    in
        fun handle_function_request reactor request_opt =
            case request_opt of
                None => reactor
              | Some (AddReadDataStream name on_read on_connect_opt on_error
                        rd_bufsz rd_lwm in_addr_opt callback error_callback) =>
                    let
                      val (new_reactor, new_request_opt) = 
                            process_add_read_data_stream_request 
                                reactor name on_read on_connect_opt on_error rd_bufsz rd_lwm in_addr_opt callback
                        handle 
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (AddWriteDataStream name on_connect_opt on_error wr_bufsz wr_lwm 
                        in_addr_opt callback error_callback) =>
                    let
                      val (new_reactor, new_request_opt) = 
                            process_add_write_data_stream_request 
                                reactor name on_connect_opt on_error wr_bufsz wr_lwm in_addr_opt callback
                        handle 
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (AddReadWriteDataStream name on_read on_connect_opt on_error 
                        rd_bufsz rd_lwm wr_bufsz wr_lwm in_addr_opt callback error_callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_add_read_write_data_stream_request 
                                reactor name on_read on_connect_opt on_error rd_bufsz rd_lwm wr_bufsz wr_lwm in_addr_opt callback
                        handle 
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (Connect fd sockaddr callback error_callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_connect_request reactor fd sockaddr callback
                        handle
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (AddAcceptor name on_accept on_error sockaddr backlog callback error_callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_add_acceptor_request 
                                reactor name on_accept on_error sockaddr backlog callback
                        handle 
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (AddTimer name initial_mcsec period_mcsec on_timer on_error callback error_callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_add_timer_request reactor name initial_mcsec period_mcsec on_timer on_error callback
                        handle 
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (SetTimer fd initial_mcsec period_mcsec callback error_callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_set_timer_request reactor fd initial_mcsec period_mcsec callback
                        handle 
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (AddReadFile name fd on_read on_error rd_bufsz rd_lwm callback error_callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_add_read_file_request reactor name fd on_read on_error rd_bufsz rd_lwm callback
                        handle
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (AddWriteFile name fd on_error rd_bufsz rd_lwm callback error_callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_add_write_file_request reactor name fd on_error rd_bufsz rd_lwm callback
                        handle
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (Write fd data callback error_callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_write_request reactor fd data callback
                        handle
                            ReactorSystemError errno => process_error reactor error_callback errno
                          | ReactorBadArgumentError => process_error reactor error_callback ~10
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some (Remove fd callback) =>
                    let
                        val (new_reactor, new_request_opt) = 
                            process_remove_request reactor fd callback
                        
                        (* No exception handling, as remove operation does not through,
                         * but just logs the exceptions. *)
                    in
                        handle_function_request new_reactor new_request_opt
                    end
              | Some ExitRun => process_exit_run reactor
    end

    (**
     *  Internal function that handles occurring during i/o errors. 
     *  It logs the error with provided information, and calls user-
     *  specified error handler.
     *
     *  @param reactor `'a reactor`: a reactor where critical error occurred.
     *  @param a_where `string`: a prefix that indicates the public reactor function 
     *      executed while an error occurred.
     *  @param fd_info `'a reactor fd_info`: information about descriptor 
     *      where error occurred.
     *  @param errno_opt `int option`: if provided is used as an errno argument
     *      in error handler. If not provided, current `Errno.errno ()` is used.
     *  @param msg `string`: a message that is appended to the end of the logs.
     *)
    fun handle_io_error reactor (a_where : string) fd_info (errno_opt : int option) (msg : string) =
        let
            val logger = ReactorType.get_logger reactor

            val fd = FdInfoType.get_fd fd_info
            val name = FdInfoType.get_name fd_info
            val errno = case errno_opt of Some errno => errno | None => Errno.errno ()

            val (ErrHandler err_handler) = FdInfoType.get_err_handler fd_info

            val to_log = "Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                ", Name=" ^ name ^ ", errno=" ^ Int.toString errno ^
                ": " ^ msg ^ "."
            val _ = Logger.error logger to_log

            (* Invoke used-defined error handler. *)
            val (new_state, new_request_opt) = err_handler (ReactorType.get_state reactor) fd errno
        in
            ReactorType.set_state reactor new_state;
            handle_function_request reactor new_request_opt
        end

    (**
     *  Internal function that handles occurring during i/o errors that requires
     *  the reactor to be terminated. It logs the error with provided information,
     *  clears and throws an `ReactorExitRun` exception.
     *
     *  @param reactor `'a reactor`: a reactor where critical error occurred.
     *  @param a_where `string`: a prefix that indicates the public reactor function 
     *      executed while an error occurred.
     *  @param fd_info `'a reactor fd_info`: information about descriptor 
     *      where error occurred.
     *  @param errno_opt `int option`: if provided is used as an errno argument
     *      in error handler. If not provided, current `Errno.errno ()` is used.
     *  @param msg `string`: a message that is appended to the end of the logs.
     *
     *  @raises `ReactorExitRun` which should be handled in the poll cycle.
     *)
    fun handle_critical_io_error reactor (a_where : string) fd_info (errno_opt : int option) (msg : string) =
        let
            val logger = ReactorType.get_logger reactor

            val fd = FdInfoType.get_fd fd_info
            val name = FdInfoType.get_name fd_info
            val errno = case errno_opt of Some errno => errno | None => Errno.errno ()

            val (ErrHandler err_handler) = FdInfoType.get_err_handler fd_info

            val to_log = "Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                ", Name=" ^ name ^ ", errno=" ^ Int.toString errno ^
                ": " ^ msg ^ "."
            val _ = Logger.critical logger to_log

            (* Invoke used-defined error handler. *)
            val (new_state, new_request_opt) = err_handler (ReactorType.get_state reactor) fd errno
        in
            ReactorType.set_state reactor new_state;
            handle_function_request reactor new_request_opt;
            raise ReactorExitRun
        end

    (**
     *  Sent out any bytes remaining into the wr_buff (which were)
     *  not sent synchronously. As it is called from writable descriptor
     *  handlers, we rely on the following preconditions:
     *      - FDInfo has not been deleted from the reactor;
     *      - FDInfo is writable.
     *  It is the responsibility of the caller to ensure that these
     *  preconditions hold.  
     *)
    fun delayed_write reactor fd_info = 
        let
            val logger = ReactorType.get_logger reactor
            
            val fd = FdInfoType.get_fd fd_info
            val wr_buff = FdInfoType.get_wr_buff fd_info
            val initial_buff_size = IOBuffer.size wr_buff
            val res = Ok (IO.write_until_eagain fd (ByteArray.empty 0) wr_buff)
                handle 
                    FFIFailure => Error FFIFailure
                  | IOBufferOverflow => Error IOBufferOverflow
        in
            case res of
                Ok n => (
                    if 
                        initial_buff_size > 0
                    then (
                        Logger.info
                            logger
                            ("Reactor.delayed_write: FD=" ^ Int.toString fd ^
                             ". Written from buffer n_buff=" ^ Int.toString n ^ " bytes.");
                        reactor
                    ) else
                        reactor
                )
              | Error FFIFailure =>
                    handle_io_error reactor "delayed_write" fd_info None "write() failed"
              | Error IOBufferOverflow =>
                    handle_io_error reactor "delayed_write" fd_info (Some (~1)) "write() failed with Buffer Overflow"
        end


    (**
     *  Internal function that handles new pending connection events at
     *  listening sockets. May terminate the reactor, if any error occurs
     *  during accepting a new connection.
     *)
    fun handle_accept reactor fd_info (events_mask : epoll_events_mask) =
        (**
         *  For Acceptor sockets, we are only interested in Readability events; also
         *  do NOT accept the connection if there was an EPoll-detected error on the
         *  Acceptor socket (it's just safer to do it this way); any other events 
         *  are silently ignored.
         *)
        if 
            not (ReactorPrivate.is_readable events_mask) 
                orelse 
            ReactorPrivate.is_error events_mask
        then
            reactor
        else
            let
                val logger = ReactorType.get_logger reactor

                val acceptor_fd = FdInfoType.get_fd fd_info
                val (AcceptHandler accept_handler) = FdInfoType.get_accept_handler fd_info

                fun accept_until_eagain r =
                    (* Acceptor can be removed from the reactor as a result of
                     * accept_handler callback. Due to that reason we have to ensure
                     * that the acceptor descriptor still exists in the reactor.
                     * If it has been deleted, we break from the accepting cycle. *)
                    if 
                        not (ReactorType.has_fd_info r acceptor_fd)
                    then 
                        r
                    else
                        let
                            val res = Ok (Socket.accept acceptor_fd)
                            handle exn => Error exn
                        in
                            case res of
                                Ok (client_fd, client_address, client_port) => (
                                    Logger.info 
                                        logger
                                        ("Reactor.handle_accept: FDListening=" ^ Int.toString acceptor_fd ^ 
                                         ". A new connection is accepted at FD=" ^ Int.toString client_fd ^
                                         ", from Address=" ^ InAddr.to_string client_address ^
                                         ", Port=" ^ Int.toString client_port ^ ".");
                                    let
                                        val (new_state, new_request_opt) = 
                                            accept_handler (ReactorType.get_state r) acceptor_fd client_fd (SockAddrIn client_address client_port)
                                    in
                                        ReactorType.set_state r new_state;
                                        accept_until_eagain (handle_function_request r new_request_opt)
                                    end
                                )
                              | Error FFIEintr => 
                                    (* Accepting call has been interrupted. Try again. *)
                                    accept_until_eagain r
                              | Error FFIEagain => 
                                    (* No more connections in the pending queue. We can return now. *)
                                    r
                              | Error FFIFailure => (
                                    (* Some unexpected error occurred while accepting a new connection.
                                     * This is a serious error condition, and we should terminate the reactor.
                                     *)
                                    handle_critical_io_error 
                                        r "handle_accept" fd_info None "accept() failed"
                              )
                        end
            in
                accept_until_eagain reactor
            end

    fun handle_timer reactor fd_info (events_mask : epoll_events_mask) =
        (* In timer we are only interested in Readability events;
         * all other events are silently ignored. *)
        if not (ReactorPrivate.is_readable events_mask)
        then reactor
        else
            let
                val logger = ReactorType.get_logger reactor
                
                val fd = FdInfoType.get_fd fd_info
                val (TimerHandler timer_handler) = FdInfoType.get_timer_handler fd_info
                val buff = FdInfoType.get_rd_buff fd_info

                fun on_read r (fd : int) (buff : io_buffer) = 
                    let
                        val data = IOBuffer.read buff 8
                        val n_expirations = MarshallingHelp.w82n_little (ByteArray.from_string data) 0
                        val (new_state, new_request_opt) = timer_handler (ReactorType.get_state r) fd n_expirations
                    in
                        ReactorType.set_state r new_state;
                        (handle_function_request r new_request_opt, String.size data)
                    end

                val result = IO.read_until_eagain reactor fd buff on_read ReactorType.has_fd_info
            in
                case result of
                    Ok (new_reactor, n_total) => (
                        Logger.info
                            logger
                            ("Reactor.handle_timer: FD=" ^ Int.toString fd ^
                            ". Read n=" ^ Int.toString n_total ^ " bytes.");
                        new_reactor
                    )
                  | Error (new_reactor, n_total, exn) => (
                        (* User-defined error handler is invloked inside 
                         * `ReactorPrivate.handle_critical_io_error` function. *)
                        case exn of 
                            FFIFailure =>
                                (* In this case, we terminate the whole Reactor because
                                 * it is a serious INTERNAL error condition. *)
                                handle_critical_io_error 
                                    reactor "handle_timer" fd_info None "read() failed"
                          | IOBufferOverflow =>
                                (*  Means that there is some logical error on the reactor
                                 *  side, as it was we who created a buffer. 
                                 *  In this case, we terminate the whole Reactor as well. *)
                                handle_critical_io_error
                                    reactor "handle_timer" fd_info (Some ~1) "read() failed with Buffer Overflow"
                          | IOEndOfFile =>
                                (* In this case, we terminate the whole Reactor because
                                 * it is a serious INTERNAL error condition. *)
                                handle_critical_io_error
                                    reactor "handle_timer" fd_info (Some ~2) "read() results in End-Of-File"
                  )
            end

    fun handle_read_file reactor fd_info (events_mask : epoll_events_mask) =
        (* In read file we are only interested in Readability events;
         * all other events are silently ignored. *)
        if not (ReactorPrivate.is_readable events_mask)
        then reactor
        else
            let
                val logger = ReactorType.get_logger reactor

                val fd = FdInfoType.get_fd fd_info
                val (ReadHandler read_handler) = FdInfoType.get_read_handler fd_info
                val buff = FdInfoType.get_rd_buff fd_info

                fun on_read r (fd : int) (buff : io_buffer) =
                    let
                        val (new_state, n, new_request_opt) = read_handler (ReactorType.get_state r) fd buff
                    in
                        ReactorType.set_state r new_state;
                        (handle_function_request r new_request_opt, n)
                    end

                val result = IO.read_until_eagain reactor fd buff on_read ReactorType.has_fd_info
            in
                case result of
                    Ok (new_reactor, n_total) => new_reactor
                  | Error (new_reactor, n_total, exn) => (
                        (* User-defined error handler is invloked inside 
                         * `ReactorPrivate.handle_critical_io_error` function. *)
                        case  exn of
                            FFIFailure => 
                                handle_io_error 
                                    new_reactor "handle_read_file" fd_info None "read() failed"
                          | IOBufferOverflow =>
                                handle_io_error
                                    new_reactor "handle_read_file" fd_info (Some (~1)) "read() failed with Buffer Overflow"
                          | IOEndOfFile =>
                                handle_io_error
                                    new_reactor "handle_read_file" fd_info (Some (~2)) "read() results in End-Of-File"
                  )
            end

    fun handle_write_file reactor fd_info (events_mask : epoll_events_mask) =
        (* In write file we are only interested in Readability events;
         * all other events are silently ignored. But we are writing only
         * if there was no previous error, or write will likely fail. *)
        if 
            not (ReactorPrivate.is_writable events_mask)
                orelse
            ReactorPrivate.is_error events_mask
        then reactor
        else delayed_write reactor fd_info
end

structure Reactor =
struct
    fun get_state reactor = ReactorType.get_state reactor

    (* 
     *  Creates an epoll descriptor and initializes the reactor. 
     *
     *  @param state: a state that the reactor will return into callback functions.
     *  @param logger `logger`: a logger that will be used to journal errors.
     *
     *  @returns `'a reactor option`: if reactor initialization succeded,
     *      and setup request did not return reactor shutdown, 'Some reactor'
     *      would be returned. Otherwise, 'None'.
     *)
    fun init state logger setup_request =
        let
            fun create () = 
                let
                    val epoll_fd = Epoll.create ()
                    handle FFIFailure => (
                        ReactorPrivate.raise_reactor_system_error (fn errno =>
                            Logger.critical 
                                logger 
                                ("Reactor.init: epoll_create() failed with Error=" ^ 
                                 Int.toString errno ^ ".")
                                )
                    ) None
                in
                    Logger.info
                        logger 
                        ("Reactor.init: reactor created at FD=" ^ Int.toString epoll_fd ^ ".");
                    ReactorPrivate.init state logger epoll_fd
                end

            fun setup reactor =
                ReactorInternal.handle_function_request reactor (Some setup_request)

            val reactor_opt = Some (create ())
            handle ReactorSystemError errno => None
        in
            Option.map setup reactor_opt
        end

    (**
     *  Clears the reactor: removes all added file descriptors from the
     *  epoll mechanism. If the descriptor was created by the 
     *  reactor (actually, for now it means that the descriptor does
     *  not reffer to a file), close the descriptor. Closes main 
     *  epoll descriptor, mark the reactor as closed, which prevents 
     *  it from further usage.
     *
     *  Any errros are ignored here, and only logged with a 'warn' level of logging.
     *
     *  @param reactor `'a reactor`: a reactor that should be cleared.
     *)
    (* fun clear reactor = ReactorPrivate.clear reactor *)


    fun poll reactor (timeout_msec : int) = 
        let
            fun handle_epoll_event reactor epoll_event =
                let
                    val logger = ReactorType.get_logger reactor
                    
                    val fd = EpollEvent.fd epoll_event
                    val events_mask = EpollEvent.events epoll_event

                    val _ = Logger.info logger 
                        ("Reactor.poll: FD=" ^ Int.toString fd ^
                         ": " ^ EpollEventsMask.to_string events_mask ^ ".")
                in
                    case ReactorType.get_fd_info_opt reactor fd of
                        None => (
                            Logger.warn 
                                logger 
                                ("Reactor.poll: OldFD=" ^ Int.toString fd ^
                                 ": No FDInfo, but still got Event=" ^ 
                                 EpollEventsMask.to_string events_mask ^ ".");
                            reactor
                        )
                      | Some fd_info => (
                            let
                                val new_reactor = 
                                    case fd_info of
                                        (* (ReadDataStreamFdInfo _ _ _ _ _ _ _ _) => reactor *)
                                        (AcceptorFdInfo _ _ _ _) =>
                                            ReactorInternal.handle_accept reactor fd_info events_mask
                                      | (TimerFdInfo _ _ _ _ _) => 
                                            ReactorInternal.handle_timer reactor fd_info events_mask
                                      | (ReadFileFdInfo _ _ _ _ _) => 
                                            ReactorInternal.handle_read_file reactor fd_info events_mask
                                      | (WriteFileFdInfo _ _ _ _ ) =>
                                            ReactorInternal.handle_write_file reactor fd_info events_mask

                                val fd = FdInfoType.get_fd fd_info
                            in
                                if ReactorPrivate.is_error events_mask
                                then
                                    if ReactorType.has_fd_info reactor fd
                                    then 
                                        (* The descriptor has not been deleted during usual event handling. 
                                         * Log the error and call the error handler. *)
                                        ReactorInternal.handle_io_error 
                                            reactor "poll" fd_info (Some 0)
                                            ("epoll_wait() returned Events=" ^ 
                                                EpollEventsMask.to_string events_mask ^ 
                                                "which include erroneous events")
                                    else 
                                        (* The descriptor has been already deleted. Just return 
                                         * the reactor. *)
                                        new_reactor 
                                else new_reactor
                            end
                        )
                end
            
            fun handle_epoll_events reactor epoll_events = 
                case epoll_events of
                    [] => ()
                  | (ev::evs) => (
                        handle_epoll_event reactor ev; 
                        handle_epoll_events reactor evs
                    )

            val logger = ReactorType.get_logger reactor
            val epoll_fd = ReactorType.get_epoll_fd reactor
            val max_events = List.length (Map.toAscList (ReactorType.get_fds reactor))

            fun wait_and_handle () =
                Epoll.wait epoll_fd max_events timeout_msec
                handle 
                    FFIEintr => (
                        (*  Avoid false exits if `epoll_wait` is interrupted.   *)
                        Logger.info logger "Reactor.poll: Interrupt occurred. Start again.";
                        wait_and_handle ()
                    )
                  | FFIFailure => (
                        (*  This is a critical error which most likely means incorrect logic.
                         *  For this reason, it is NOT passed to "ErrHandler" and not returned as
                         *  an error flag: Throw an exception instead
                         *)
                        Logger.critical
                            logger
                            ("Reactor.poll: epoll_wait() failed with Error=" ^ 
                             Int.toString (Errno.errno()) ^ ".");
                        raise ReactorExitRun
                    )
            val epoll_events = wait_and_handle ()
        in
            Logger.info 
                logger 
                ("Rector.poll: epoll_wait() returned with " ^ 
                 Int.toString (List.length epoll_events) ^ " ready descriptors.");
            handle_epoll_events reactor epoll_events
        end

    fun run reactor =
        let
            val timeout = ~1

            fun loop () = (
                poll reactor timeout;
                loop ()
            )
        in
            if True
            then 
                loop ()
                handle ReactorExitRun => ReactorPrivate.clear reactor
            else ()
        end
end

val _ = 
    Exception.add_exn_name_printer 
        (fn e =>
            case e of
                ReactorExitRun => "ReactorExitRun"
              | ReactorSystemError _ => "ReactorSystemError"
              | ReactorBadArgumentError => "ReactorBadArgumentError"
              | _ => raise Exception.Unknown
        )
val _ = 
    Exception.add_exn_message_printer
        (fn e =>
            case e of
                ReactorExitRun => "ReactorExitRun"
              | ReactorSystemError v => "ReactorSystemError" ^ " " ^ Int.toString v
              | ReactorBadArgumentError => "ReactorBadArgumentError"
              | _ => raise Exception.Unknown
        )


(* val k = Ref 0
fun on_timer s fd n = (k := (!k) + 1; print ("\n\n===ON_TIMER=" ^ Int.toString (Time.current_msec ()) ^ ", K=" ^ Int.toString (!k) ^ "==\n\n"); (s, if (!k) > 20 then Some ExitRun else None))
fun on_error s fd = (print ("\n\n===ON_ERROR===\n\n"); (s, None))

fun callback s fd = (print ("\n\n====CALLBACK===\n\n"); (s, None))
fun error_callback s errno = (print ("\n\n====ERROR_CALLBACK===\n\n"); (s, None))

val setup_request = AddTimer "timer" 2000000 5000000 (TimerHandler on_timer) (ErrHandler on_error) callback error_callback

val logger = Logger.create TextIO.stdOut LoggerLevel.Info
val reactor = Option.valOf (Reactor.init 2 logger setup_request)
val reactor = Reactor.run reactor *)

(* val a = Ref 0
fun on_timer s fd n = (a := (!a) + 1; print ("\n\n===ON_TIMER: FD=" ^ Int.toString fd ^ ", N=" ^ Int.toString n ^ ", A=" ^ Int.toString (!a) ^ "===\n\n"))
fun on_error s fd = print ("\n\n===ON_ERROR: FD=" ^ Int.toString fd ^ "===\n\n")

val reactor = Reactor.init 2 logger
val fd = Reactor.add_timer reactor "test" 200000 10000000 on_timer on_error

val _ = Reactor.run reactor
handle exn => (print ("\n===EXCEPTION=" ^ Exception.exn_message exn ^ "===\n"); raise exn) *)
