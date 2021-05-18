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
 *)
datatype 'a reactor = Reactor ('a ref) logger int ((int, ('a reactor) fd_info) map ref)

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
    fun get_state (Reactor state_ref _ _ _) = (!state_ref)
    fun get_logger (Reactor _ logger _ _) = logger
    fun get_epoll_fd (Reactor _ _ epoll_fd _) = epoll_fd
    fun get_fds (Reactor _ _ _ fds_ref) = (!fds_ref)

    fun set_state (Reactor state_ref _ _ _) state = 
        state_ref := state
    fun set_logger (Reactor state_ref _ epoll_fd fds_ref) logger = 
        Reactor state_ref logger epoll_fd fds_ref
    fun set_epoll_fd (Reactor state_ref logger _ fds_ref) epoll_fd =
        Reactor state_ref logger epoll_fd fds_ref
    fun set_fds (Reactor _ _ _ fds_ref) fds =
        fds_ref := fds

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
end

structure ReactorPrivate =
struct
    (* Initializes the reactor. *)
    fun init state logger epoll_fd =
        Reactor (Ref state) logger epoll_fd (Ref (Map.empty Int.compare))

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

            val fd_info = TimerFdInfo name fd on_timer on_error
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


    (* fun poll reactor (timeout_msec : int) = () *)
end

fun on_timer s fd = ()
fun on_error s fd = ()

val logger = Logger.create TextIO.stdOut LoggerLevel.Info
val reactor = Reactor.init 2 logger
val fd = Reactor.add_timer reactor "test" 0 0 on_timer on_error
(* val _ = print ("\n====" ^ Int.toString (Errno.errno ()) ^ "=====\n") *)