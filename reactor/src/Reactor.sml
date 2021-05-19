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
datatype 'a reactor = Reactor ('a ref) logger int ((int, ('a reactor) fd_info) map ref) (bool ref)

(**
 *  Is raised to stop the reactor and exit from the run function.
 *  There is currently no other way of exiting from the "Run" infinite loop
 *  but to throw the following exception; so this is NOT actually an error condition!
 *)
exception ReactorExitRun

(**
 *  Is raised if any FFI call returns an error.
 *)
exception ReactorSystemError

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

    (* Adds a new fd_info into the reactor. *)
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
             Errno.strerror (Errno.errno()))
    )

    (**
     *  Internal function that removes a descriptor from the epoll
     *  mechanism, close the descriptor, and remove corresponding
     *  fd_info from reactor's internal structure.
     *
     *  Any errros are ignored here, and only logged with a 'warn' level of logging.
     *
     *  @param reactor `'a reactor`: a reactor the file descriptor should be
     *      removed from.
     *  @param a_where `string`: a prefix that indicates the public reactor function
     *      that initiated the removal.
     *  @param fd `int`: a fd that should be removed.
     *)
    fun remove_from_epoll reactor (a_where : string) (fd : int) =
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
                     ". Detaching failed with " ^ Errno.errno_strerror () ^ ".")
            );

            (* Close the file descriptor. Ignore any error here. *)
            close_fd logger a_where fd;

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
     *
     *  @raises `ReactorSystemError` if call to `epoll_ctl` fails. In that case the socket
     *      will not be closed, and the caller function is responsible for the exception
     *      handling and closing the socket if required.
     *)
    fun add_to_epoll reactor (a_where : string) fd_info (events_mask : epoll_events_mask) =
        let
            val logger = ReactorType.get_logger reactor
            val epoll_fd = ReactorType.get_epoll_fd reactor

            val fd = FdInfoType.get_fd fd_info
        in
            Epoll.ctl EpollCtlAdd epoll_fd fd events_mask
            handle FFIFailure => (
                Logger.error
                    logger
                    ("Reactor." ^ a_where ^ ": Adding FD=" ^ Int.toString fd ^ 
                     ", Name=" ^ FdInfoType.get_name fd_info ^ 
                     ", HType=" ^ FdInfoType.get_handler_type fd_info ^ ". Failed.");
                raise ReactorSystemError
            );
            Logger.info 
                logger 
                ("Reactor." ^ a_where ^ ": Added FD=" ^ Int.toString fd ^ 
                 ", Name=" ^ FdInfoType.get_name fd_info ^ 
                 ", HType=" ^ FdInfoType.get_handler_type fd_info ^ ".");
            
            ReactorType.add_fd_info reactor fd_info
        end

    (**
     *  Internal function that handles occuring errors. It logs the error 
     *  with provided information, and additionally may stop the reactor
     *  completely or throw any exception.
     *
     *  @param stop_reactor `bool`: indicates whether the reactor should be stopped.
     *  @param exn_opt `exception option`: optional exception that should be raised.
     *      If 'stop_reactor' parameter is specified, the parameter is just ignored.
     *  @param log_f `string -> unit`: a function that logs the error. Caller should
     *      specifiy a logging level and a logger to be used before.
     *      (E.g. `Logger.error logger`)
     *  @param a_where `string`: a prefix that indicates the public reactor function 
     *      executed while error occurred.
     *  @param fd_info_opt `'a reactor fd_info option`: optional information about 
     *      descriptor where error occurred. If provided adds fd and name of the 
     *      descriptor to the logs.
     *  @param fmt_errno `bool`: indicates whether errno parameter and its description 
     *      should be added to the logs.
     *  @param msg `string`: a message that is appended to the end of the logs.
     *
     *  @raises exception provided in \a exn_opt parameter by the caller, if any.
     *)
    (* local
        fun handle_error (stop_reactor : bool) exn_opt (log_f : string -> unit)
                (a_where : string) fd_info_opt (fmt_errno : bool) (msg : string) =
            let
                val to_log = "Reactor." ^ a_where ^ ": "
            in
                ()
            end
    in *)
    
    (**
     *  Internal implementation of the public `clear` function.
     *
     *  Any errros are ignored here, and only logged with a 'warn' level of logging.
     *
     *  @param reactor `'a reactor`: a reactor that should be cleared.
     *)
    fun clear reactor =
        let
            fun remove_all_fds (fds_list : int list) =
                case fds_list of
                    [] => ()
                  | (fd :: others) => (
                        remove_from_epoll reactor "clear" fd;
                        remove_all_fds others
                    )
            val fds_list = List.map fst (Map.toAscList (ReactorType.get_fds reactor))

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
     *  Internal function that handles occurring during i/o errors that requires
     *  the reactor to be terminated. It logs the error with provided information,
     *  clears and marks the reactor as non-usable, and throws an `ReactorExitRun`
     *  exception.
     *
     *  @param reactor `'a reactor`: a reactor where critical error occurred.
     *  @param a_where `string`: a prefix that indicates the public reactor function 
     *      executed while an error occurred.
     *  @param fd_info `'a reactor fd_info`: information about descriptor 
     *      where error occurred.
     *  @param msg `string`: a message that is appended to the end of the logs.
     *
     *  @raises `ReactorExitRun` which should be handled in the poll cycle.
     *)
    fun handle_critical_io_error reactor (a_where : string) fd_info (msg : string) =
        let
            val logger = ReactorType.get_logger reactor

            val fd = FdInfoType.get_fd fd_info
            val name = FdInfoType.get_name fd_info
            val errno = Errno.errno ()
            val errno_strerror = Errno.strerror errno

            val err_handler = FdInfoType.get_err_handler fd_info

            val to_log = "Reactor." ^ a_where ^ ": FD=" ^ Int.toString fd ^
                ", Name=" ^ name ^ ", errno=" ^ Int.toString errno ^
                ", desc=" ^ errno_strerror ^ ": " ^ msg ^ "."
        in
            Logger.critical logger to_log;
            (* Invoke used-defined error handler. *)
            err_handler reactor fd;
            (* Terminate the reactor. *)
            clear reactor;
            raise ReactorExitRun
        end
end

structure Reactor =
struct
    (* 
     *  Creates an epoll descriptor and initializes the reactor. 
     *
     *  @param state: a state that the reactor will return into callback functions.
     *  @param logger `logger`: a logger that will be used to journal errors.
     *
     *  @raises `ReactorSystemError` if epoll initialization returns an error.
     *)
    fun init state logger =
        let
            val epoll_fd = Epoll.create ()
            handle FFIFailure => (
                Logger.critical 
                    logger 
                    ("Reactor.init: epoll_create() failed with " ^ 
                     Errno.strerror (Errno.errno ()) ^ ".");
                raise ReactorSystemError
            )
        in
            Logger.info 
                logger 
                ("Reactor.init: reactor created at FD=" ^ Int.toString epoll_fd ^ ".");
            ReactorPrivate.init state logger epoll_fd
        end

    (**
     *  Clears the reactor: removes all added file descriptors from the
     *  epoll mechanism and closes them. Closes main epoll descriptor,
     *  mark the reactor as closed, which prevents it from further usage.
     *
     *  Any errros are ignored here, and only logged with a 'warn' level of logging.
     *
     *  @param reactor `'a reactor`: a reactor that should be cleared.
     *)
    fun clear reactor = ReactorPrivate.clear reactor

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
                Logger.error
                    logger
                    ("Reactor.add_timer: create_timer() failed with " ^
                     Errno.errno_strerror () ^ ". Name=" ^ name ^ ".");
                raise ReactorSystemError
            )
            val _ = Logger.info logger 
                ("Reactor.add_timer: timer created at FD=" ^ Int.toString fd ^ ". Name=" ^ name ^ ".")

            val _ = Timer.set_time fd initial_mcsec period_mcsec
            handle FFIFailure => (
                Logger.error
                    logger
                    ("Reactor.add_timer: set_timer() failed with " ^
                     Errno.errno_strerror () ^ ". FD=" ^ Int.toString fd ^ ".");
                ReactorPrivate.close_fd logger "add_timer" fd;
                raise ReactorSystemError
            )
            val _ = Logger.info logger
                ("Reactor.add_timer: FD=" ^ Int.toString fd ^ ". Timer was set.")

            (*  We are allowing the buffer to handle 31 expirations without crunching. 
             *  When a timer epires 31 times, there are only 8 bytes left to read the data,
             *  and, thus, we cannot detect EAGAIN condition while reading 32nd expiration,
             *  without crunching the buffer before.
             *)
            val buff = IOBuffer.init (8 * 32) 9
            val fd_info = TimerFdInfo name fd on_timer on_error buff
            val events_mask = EpollEventsMask.from_list [Epollet, Epollin]
        in
            ReactorPrivate.add_to_epoll reactor "add_timer" fd_info events_mask
            handle ReactorSystemError => (
                (* Addition to the epoll mechanism failed. Close
                 * the opened descriptor and propagate the exception. 
                 *)
                ReactorPrivate.close_fd logger "add_timer" fd;
                raise ReactorSystemError
            )
            fd
        end

    fun handle_timer reactor fd_info (events_mask : epoll_events_mask) =
        (* In timer we are only interested in Readability events;
         * all other events are silently ignored. *)
        if not (ReactorPrivate.is_readable events_mask)
        then ()
        else
            let
                val logger = ReactorType.get_logger reactor
                
                val fd = FdInfoType.get_fd fd_info
                val timer_handler = FdInfoType.get_timer_handler fd_info
                val buff = FdInfoType.get_rd_buff fd_info

                val n = IO.read_until_eagain fd buff
                    handle
                        FFIFailure =>
                            (* In this case, we terminate the whole Reactor because
                             * it is a serious INTERNAL error condition. *)
                            ReactorPrivate.handle_critical_io_error 
                                reactor "handle_timer" fd_info "read() failed"
                      | IOBufferOverflow =>
                            (*  Means that there is some logical error on the reactor
                             *  side, as it was we who created a buffer. 
                             *  In this case, we terminate the whole Reactor as well. *)
                            ReactorPrivate.handle_critical_io_error
                                reactor "handle_timer" fd_info "read() failed with Buffer Overflow"
                val n_expirations = MarshallingHelp.w82n_little (ByteArray.from_string (IOBuffer.read buff n)) 0
            in
                Logger.info
                    logger
                    ("Reactor.handle_timer: FD=" ^ Int.toString fd ^
                     ". Read n=" ^ Int.toString n ^ " bytes.");
                timer_handler reactor fd n_expirations
            end


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
                                 EpollEventsMask.to_string events_mask ^ ".")
                        )
                      | Some fd_info => (
                            case fd_info of
                                (TimerFdInfo _ _ _ _ _) => (
                                    handle_timer reactor fd_info events_mask;
                                    if ReactorPrivate.is_error events_mask
                                    then ()
                                    else ()
                                )
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
                            ("Reactor.poll: epoll_wait() failed with " ^
                             Errno.errno_strerror () ^ ".");
                        raise ReactorSystemError
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
                handle ReactorExitRun => ()
            else ()
        end
end

fun on_timer s fd n = print ("\n\n===ON_TIMER: FD=" ^ Int.toString fd ^ ", N=" ^ Int.toString n ^ "===\n\n")
fun on_error s fd = print ("\n\n===ON_ERROR: FD=" ^ Int.toString fd ^ "===\n\n")

val logger = Logger.create TextIO.stdOut LoggerLevel.Info
val reactor = Reactor.init 2 logger
val fd = Reactor.add_timer reactor "test" 2000000  9000000 on_timer on_error

val _ = Reactor.run reactor
handle exn => (print ("\n===EXCEPTION=" ^ Exception.exn_message exn ^ "===\n"); raise exn)

