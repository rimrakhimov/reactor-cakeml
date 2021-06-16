datatype client_state = 
    ClientState int ((int, bool) map) ((int, sockaddr_in) map) ((int, string) map) ((int, int) map) ((int, int) map) ((int * int) list)

structure ClientState =
struct
    fun init number_of_clients = 
        ClientState
            number_of_clients
            (Map.empty Int.compare)
            (Map.empty Int.compare)
            (Map.empty Int.compare)
            (Map.empty Int.compare)
            (Map.empty Int.compare)
            []

    fun get_number_of_clients (ClientState number_of_clients _ _ _ _ _ _) =
        number_of_clients
    fun get_completed_clients (ClientState _ completed_clients _ _ _ _ _) =
        completed_clients
    fun get_fd_to_address_map (ClientState _ _ fd_to_address_map _ _ _ _) = 
        fd_to_address_map
    fun get_fd_to_msg_map (ClientState _ _ _ fd_to_msg_map _ _ _) = 
        fd_to_msg_map
    fun get_neighbour_port_to_original_fd (ClientState _ _ _ _ neighbour_port_to_original_fd _ _) =
        neighbour_port_to_original_fd
    fun get_neighbour_fd_to_original_fd (ClientState _ _ _ _ _ neighbour_fd_to_original_fd _) =
        neighbour_fd_to_original_fd
    fun get_ports_to_connect (ClientState _ _ _ _ _  _ ports_to_connect) =
        ports_to_connect

    fun set_completed_clients (ClientState number_of_clients _ fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect) completed_clients = 
        ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect
    fun set_fd_to_address_map (ClientState number_of_clients completed_clients _ fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect) fd_to_address_map = 
        ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect
    fun set_fd_to_msg_map (ClientState number_of_clients completed_clients fd_to_address_map _ neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect) fd_to_msg_map = 
        ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect
    fun set_neighbour_port_to_original_fd (ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map _ neighbour_fd_to_original_fd ports_to_connect) neighbour_port_to_original_fd =
        ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect
    fun set_neighbour_fd_to_original_fd (ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd _ ports_to_connect) neighbour_fd_to_original_fd =
        ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect
    fun set_ports_to_connect (ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd _) ports_to_connect =
        ClientState number_of_clients completed_clients fd_to_address_map fd_to_msg_map neighbour_port_to_original_fd neighbour_fd_to_original_fd ports_to_connect

    (****************************************************************)

    fun add_completed_client state client_fd = 
        set_completed_clients state (Map.insert (get_completed_clients state) client_fd True)
    fun get_completed_clients_number state =
        List.length (Map.toAscList (get_completed_clients state))

    fun add_new_address state fd address = 
        set_fd_to_address_map state (
            Map.insert (get_fd_to_address_map state) fd address
        )
    fun get_address_by_fd_opt state fd =
        Map.lookup (get_fd_to_address_map state) fd
    
    fun add_new_msg state fd msg =
        set_fd_to_msg_map state (
            Map.insert (get_fd_to_msg_map state) fd msg
        )
    fun get_msg_by_fd_opt state fd =
        Map.lookup (get_fd_to_msg_map state) fd
    
    fun add_to_ports_to_connect state (port, original_fd) =
        set_ports_to_connect state (
            (port, original_fd) :: (get_ports_to_connect state)
        )
    fun clear_ports_to_connect state = set_ports_to_connect state []

    fun add_original_fd_to_neighbour_fd state neighbour_fd original_fd =
        set_neighbour_fd_to_original_fd state (
            Map.insert (get_neighbour_fd_to_original_fd state) neighbour_fd original_fd
        )
    fun get_original_fd_by_neighbour_fd_opt state neighbour_fd =
        Map.lookup (get_neighbour_fd_to_original_fd state) neighbour_fd


    (* fun add_original_fd_by_neighbour_port state neighbour_port original_fd =  *)
        

    (* fun add_original_fd state neighbour_fd original_fd =
        set_neighbour_fd_to_original state (
            Map.insert (get_neighbour_fd_to_original state) neighbour_fd original_fd
        )
    fun get_neighbour_fd_by_original_opt state neighbour_fd =
        Map.lookup (get_neighbour_fd_to_original state) neighbour_fd *)


end
