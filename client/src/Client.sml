structure Client =
struct
    val rd_bufsz = 1000 
    val rd_lwm = 500 
    val wr_bufsz = 0 
    val wr_lwm = 0

    fun error_callback state errno =
        Assert.fail "Error occured"

    fun on_error state fd errno = 
        Assert.fail ("Error occured on Fd=" ^ Int.toString fd)

    (**
     *  original_fd: get_original_fd_by_neighbour_fd_opt
     *  original_text: get_msg_by_fd_opt
     *)
    fun neighbour_on_read state neighbour_fd buff =
        let
            fun is_read_finished original_text neighbour_text =
                if
                    not (String.isPrefix neighbour_text original_text)
                then
                    Assert.fail "Invalid message has been read"
                else
                    String.compare neighbour_text original_text = Equal

            val original_fd = Option.valOf (ClientState.get_original_fd_by_neighbour_fd_opt state neighbour_fd)
            val original_text = Option.valOf (ClientState.get_msg_by_fd_opt state original_fd)

            val neighbour_text = Option.getOpt (ClientState.get_msg_by_fd_opt state neighbour_fd) ""

            val msg_size = IOBuffer.size buff
			val new_msg = IOBuffer.read buff msg_size
            
            val updated_neighbour_text = neighbour_text ^ new_msg
            val updated_state = ClientState.add_new_msg state neighbour_fd updated_neighbour_text
        in
            if 
                is_read_finished original_text updated_neighbour_text
            then
                let
                    val number_of_clients = ClientState.get_number_of_clients updated_state

                    val new_state = ClientState.add_completed_client updated_state neighbour_fd
                    val number_of_completed_clients = ClientState.get_completed_clients_number new_state
                in
                    if 
                        number_of_completed_clients < number_of_clients
                    then
                        (new_state, msg_size, None)
                    else
                        (new_state, msg_size, Some ExitRun)
                end
            else
                (updated_state, msg_size, None)
        end

    fun original_on_read state original_fd buff =
        if
            (IOBuffer.size buff < 4)
        then
            (state, 0, None)
        else
            let
                val data = ByteArray.from_string (IOBuffer.read buff 4)
                val neighbour_port = MarshallingHelp.w42n data 0
            in
                (ClientState.add_to_ports_to_connect state (neighbour_port, original_fd), 4, None)
            end

    fun on_accept state acceptor_fd client_fd client_address = 
        let
            val (SockAddrIn client_addr client_port) = client_address

            val name = InAddr.to_string client_addr ^ ":" ^ Int.toString client_port

            fun add_existing_read_write_data_stream_callback state =
                let
                    val msg = "Hello " ^ (Int.toString client_port)
                    val msg_len = String.size msg
                    val data = ByteArray.empty (1 + msg_len)
                    val _ = Word8Array.update data 0 (Word8.fromInt msg_len)
                    val _ = Word8Array.copyVec msg 0 msg_len data 1
                    
                    fun write_callback state = (ClientState.add_new_msg state client_fd msg, None)
                    val req = Write client_fd data write_callback error_callback
                in
                    (ClientState.add_new_address state client_fd client_address, Some req)
                end

            val req = AddExistingReadWriteDataStream name client_fd
                (ReadHandler original_on_read) (ErrHandler on_error) 
                rd_bufsz rd_lwm wr_bufsz wr_lwm
                add_existing_read_write_data_stream_callback error_callback
        in
            (state, Some req)
        end

    (**
     *  ports: get_ports_to_connect
     *  client_address: get_address_by_fd_opt
     *)
    fun on_timer state fd n_exp =
        let
            val ports = ClientState.get_ports_to_connect state

            fun process_timer ports state = 
                case ports of
                    [] => (ClientState.clear_ports_to_connect state, None)
                  | ((port, original_fd)::ps) =>
                    let
                        val (SockAddrIn client_addr client_port) = 
                            Option.valOf (ClientState.get_address_by_fd_opt state original_fd)
                        val name = InAddr.to_string client_addr ^ ":" ^ Int.toString client_port ^ 
                                        "-" ^ Int.toString port

                        fun add_read_data_stream_callback state neighbour_fd = 
                            let
                                fun connect_callback state = 
                                    process_timer ps state
                                val req = Connect neighbour_fd (SockAddrIn client_addr port) 
                                            connect_callback error_callback
                            in
                                (ClientState.add_original_fd_to_neighbour_fd state neighbour_fd original_fd, Some req)
                            end

                        val req = 
                            AddReadDataStream name 
                                (ReadHandler neighbour_on_read) None (ErrHandler on_error) 
                                rd_bufsz rd_lwm None add_read_data_stream_callback error_callback
                    in
                        (state, Some req)
                    end
        in
            process_timer ports state
        end

    fun init acceptor_port number_of_clients =
        let
            val timer_period_mcsec = 1000000

            val state = ClientState.init number_of_clients
            val logger = StdOutLogger.init LoggerLevel.Info

            fun add_timer_callback state fd =
                (state, None)
            fun add_acceptor_callback state fd = 
                let
                    val req = AddTimer "timer" timer_period_mcsec timer_period_mcsec
                        (TimerHandler on_timer) (ErrHandler on_error)
                        add_timer_callback error_callback
                in
                    (state, Some req)
                end
            val setup_req = AddAcceptor "acceptor" (AcceptHandler on_accept)
                    (ErrHandler on_error) (SockAddrIn (InAddr.from_string "0.0.0.0") acceptor_port) number_of_clients
                    add_acceptor_callback error_callback
            val reactor = Option.valOf (Reactor.init state logger setup_req)
        in
            Reactor.run reactor
        end
end

val _ = Client.init 12345 10
