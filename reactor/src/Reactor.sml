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
 *  Structure defines getters and setters for variables in reactor representation.
 *)
structure ReactorType =
struct
    (* Initializes the reactor. *)
    fun init state logger epoll_fd =
        Reactor state logger epoll_fd (Map.empty Int.compare) (Ref None)

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
