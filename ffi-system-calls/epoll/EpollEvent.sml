(*  The type enumberates possible event types.
 *  Represents a single event type.
 *)
datatype epoll_event_enum = 
    Epollin | Epollout | Epollrdhup | Epollpri | Epollerr | Epollhup | Epollet | Epolloneshot

(**
 *  Structure defines functions that should be used to
 *  manipulate with epoll_event_enum type variables.
 *)
structure EpollEventEnum =
struct
    fun to_string ev =
        case ev of
            Epollin => "EPOLLIN"
          | Epollout => "EPOLLOUT"
          | Epollrdhup => "EPOLLRDHUP"
          | Epollpri => "EPOLLPRI"
          | Epollerr => "EPOLLERR"
          | Epollhup => "EPOLLHUP"
          | Epollet => "EPOLLET"
          | Epolloneshot => "EPOLLONESHOT"
    
    fun to_int ev =
        case ev of
            Epollin => 1
          | Epollout => 2
          | Epollrdhup => 4
          | Epollpri => 8
          | Epollerr => 16
          | Epollhup => 32
          | Epollet => 64
          | Epolloneshot => 128
end

(* The type represents a composition of single epoll events. *)
type epoll_events_mask = Word8.word

(**
 *  Structure defines functions that should be used to
 *  manipulate with epoll_events_mask type variables.
 *)
structure EpollEventsMask =
struct
    (* Number of bytes required to represent an epoll events mask type as a byte_array. *)
    val size = 1

    (* Initializes an empty epoll events mask. *)
    fun empty () = Word8.fromInt 0

    (**
     *  Adds a new event to the epoll events mask.
     *
     *  @param mask `epoll_events_mask`: mask an event should be added to.
     *  @param ev `epoll_event_enum`: the event that should be added.
     *
     *  @returns `epoll_events_mask`: updated mask.
     *)
    fun add (mask : epoll_events_mask) (ev : epoll_event_enum) = 
        Word8.orb mask (Word8.fromInt (EpollEventEnum.to_int ev))

    (**
     *  Removes an event from the epoll events mask. If an event is not
     *  in the mask the mask remains unchanged.
     *
     *  @param mask `epoll_events_mask`: mask an event should be removed from.
     *  @param ev `epoll_event_enum`: the event that should be removed.
     *
     *  @returns `epoll_events_mask`: updated mask.
     *)
    fun remove (mask : epoll_events_mask) (ev : epoll_event_enum) = 
        let
            val t = Word8.notb (Word8.fromInt (EpollEventEnum.to_int ev))
        in
            Word8.andb mask t
        end

    (**
     *  Check whether specified event is in the epoll events mask.
     *
     *  @param mask `epoll_events_mask`: mask presence of the event 
     *      should be checked in.
     *  @param ev `epoll_event_enum`: an event that should be checked.
     *
     *  @returns `bool`: true if the event is in the mask, or
     *      false otherwise.
     *)
    fun check (mask : epoll_events_mask) (ev : epoll_event_enum) =
        let
            val t = Word8.fromInt (EpollEventEnum.to_int ev)
        in
            Word8.toInt (Word8.andb mask t) > 0
        end

    (**
     *  Initializes a mask which consists of epoll events specified 
     *  which are in the list.
     *
     *  @param event_enums `epoll_event_enum list`: events that
     *      should be in the mask.
     *
     *  @returns `epoll_events_mask`: resultant mask.
     *)
    fun from_list (event_enums : epoll_event_enum list) =
        List.foldl add (empty ()) event_enums

    (**
     *  Represents an epoll events mask as a text value.
     *  Enumerates active events through a comma. If no event is in
     *  the maks, returns "{}".
     *)
    fun to_string (mask : epoll_events_mask) =
        let
            val event_types = [Epollin, Epollout, Epollrdhup, Epollpri, Epollerr, Epollhup, Epollet, Epolloneshot]
            val filtered_event_types = List.filter (check mask) event_types
            val str = String.concatWith ", " (List.map EpollEventEnum.to_string filtered_event_types)
        in
            "{" ^ str ^ "}"
        end

    (**
     *  Serializes an epoll events mask into a byte array.
     *
     *  @param mask `epoll_events_mask`: a mask to serialize.
     *
     *  @returns `byte_array`: serialized epoll events mask.
     *)
    fun to_bytes (mask : epoll_events_mask) =
        Word8Array.array 1 mask

    (**
     *  Deserializes a byte array into an epoll events mask.
     *
     *  @param b `byte_array`: data to be deserialized.
     *  @param i `int`: index in data which corresponds to the 
     *      beginnning of a serialized mask.
     *
     *  @returns `epoll_events_mask`: deserialized mask.
     *)
    fun from_bytes (b : byte_array) i =
        Word8Array.sub b i
end

(** 
 *  The type represents a pair consisting of a file descriptor and
 *  a corresponding epoll events mask, activated on the descriptor.
 *
 *  Is used to return events occurred for the corresponding file
 *  descriptor.
 *
 *  @param fd `int`: a file descriptor where events occurred.
 *  @param mask `epoll_events_mask`: a bit mask representing
 *      events happened on the descriptor.
 *)
datatype epoll_event = EpollEvent int epoll_events_mask

(**
 *  Structure defines functions that should be used to
 *  manipulate with epoll_event type variables.
 *)
structure EpollEvent =
struct
    val size = 4 + EpollEventsMask.size

    fun fd (EpollEvent fd _) = fd
    
    fun events (EpollEvent _ events) = events

    fun to_string (EpollEvent fd events) =
        "EpollEvent " ^ Int.toString fd ^ " " ^ EpollEventsMask.to_string events

    fun from_bytes (b : byte_array) (i : int) =
        EpollEvent (MarshallingHelp.w42n b i) (EpollEventsMask.from_bytes b (i + 4))
end
