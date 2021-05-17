(**
 *  The type defines internal representation of the reactor.
 *
 *  @param state `'a`: a parametric value that represents a client application state.
 *  @param logger `logger`.
 *  @param poll_fd `int`: the file descriptor corresponding to the epoll mechanism.
 *  @param fds `(int, ('a reactor) fd_info) map`: a map from file descriptors to
 *      corresponding fd_info structures.
 *  @param saved_opt_ref `('a reactor option) ref`: a reference where reactor
 *      should be saved by (is used for exiting and errors handling purposes).
 *)
datatype 'a reactor = Reactor 'a logger int ((int, ('a reactor) fd_info) map) (('a reactor option) ref)

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
    fun get_state (Reactor state _ _ _ _) = state
    fun get_logger (Reactor _ logger _ _ _) = logger
    fun get_epoll_fd (Reactor _ _ epoll_fd _ _) = epoll_fd
    fun get_fds (Reactor _ _ _ fds _) = fds
    fun get_saved_opt_ref (Reactor _ _ _ _ saved_opt_ref) = saved_opt_ref

    fun set_state (Reactor _ logger epoll_fd fds saved_opt_ref) state = 
        Reactor state logger epoll_fd fds saved_opt_ref
    fun set_logger (Reactor state _ epoll_fd fds saved_opt_ref) logger = 
        Reactor state logger epoll_fd fds saved_opt_ref
    fun set_epoll_fd (Reactor state logger _ fds saved_opt_ref) epoll_fd =
        Reactor state logger epoll_fd fds saved_opt_ref
    fun set_fds (Reactor state logger epoll_fd _ saved_opt_ref) fds =
        Reactor state logger epoll_fd fds saved_opt_ref

    (* Saves a reactor by the reference. *)
    fun save reactor = 
        get_saved_opt_ref reactor := Some reactor
    (* Removes a reactor from the reference. *)
    fun waste reactor = 
        get_saved_opt_ref reactor := None

    (* Returns fd info if specified descriptor exists in the reactor. *)
    fun get_fd_info_opt reactor fd =
        Map.lookup (get_fds reactor) fd
    
    (* Returns whether specified descriptor exists in the reactor. *)
    fun has_fd_info reactor fd =
        Option.isSome (get_fd_info_opt reactor fd)
end

structure ReactorPrivate =
struct
    (* Initializes the reactor. *)
    fun init state logger epoll_fd =
        Reactor state logger epoll_fd (Map.empty Int.compare) (Ref None)

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
end

structure Reactor =
struct
    (* 
     *  Creates an epoll descriptor and initializes the reactor. 
     *
     *  @param state: a state that the reactor will return into callback functions.
     *  @param logger `logger`: a logger that will be used to journal errors.
     *
     *  @raises ReactorSystemError if epoll initialization returns an error.
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
     *  @raises ReactorSystemError if timer creation or setting returns an error.
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
                     Errno.strerror (Errno.errno ()) ^ ". Name=" ^ name ^ ".");
                raise ReactorSystemError
            )
            val _ = Logger.info logger 
                ("Reactor.add_timer: timer created at FD=" ^ Int.toString fd ^ ". Name=" ^ name ^ ".")

            val _ = Timer.set_time fd initial_mcsec period_mcsec
            handle FFIFailure => (
                Logger.error
                    logger
                    ("Reactor.add_timer: set_timer() failed with " ^
                     (Errno.strerror (Errno.errno())) ^ ". FD=" ^ Int.toString fd ^ ".");
                ReactorPrivate.close_fd logger "add_timer" fd;
                raise ReactorSystemError
            )
        in
            ()
        end
end

val logger = Logger.create TextIO.stdOut LoggerLevel.Info
val reactor = Reactor.init 2 logger
(* val _ = print ("\n====" ^ Int.toString (Errno.errno ()) ^ "=====\n") *)