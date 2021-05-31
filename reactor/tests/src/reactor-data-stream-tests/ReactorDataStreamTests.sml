structure ReactorDataStreamTests =
struct
    val logger = StdOutLogger.init LoggerLevel.Off

	fun on_error state fd errno =
			Assert.fail "Error occured"

    fun error_callback state errno = 
        Assert.fail "Error occured"

	local
		fun add_data_stream_callback sockaddr state fd =
			let
				fun connect_callback state = (state, None)
			in
				(state, Some (Connect fd sockaddr connect_callback error_callback))
			end
	in
    	fun test_read_data_stream_can_connect_to_acceptor () =
		let
			fun validate state = 
				Assert.assertTrue state

			val address = InAddr.from_string "127.0.0.1"
			val port = 12345
			val sockaddr = SockAddrIn address port

			val inbuf = ByteArray.empty 2
			val _ = Marshalling.n2w2 port inbuf 0
			val outbuf = ByteArray.empty 0

			val _ = #(test_data_stream_can_connect_to_acceptor) (ByteArray.to_string inbuf) outbuf

			fun on_read state fd buff = (
				(* Read handler should not be called, as the connect handler
				 * should be called first, that exits the reactor.
				 *)
				Assert.fail "Read handler called"
			)
			
			fun on_connect state fd = (
				(* We successfully connected to a socket.
				 * As it is exactly what we would like to test,
				 * just terminate the reactor. *)
				(True, Some ExitRun)
			)

			(* Indicates whether on_connect callback function has been called. *)
			val state = False
			val setup_req = AddReadDataStream "read_data_stream" (ReadHandler on_read) 
						(Some (ConnectHandler on_connect)) (ErrHandler on_error) 
						100 100 None (add_data_stream_callback sockaddr) error_callback
			val r = Option.valOf (Reactor.init state logger setup_req)
		in
			Reactor.run r;
			validate (Reactor.get_state r);
			#(reactor_acceptor_tests_sleep) 
				(ByteArray.to_string (Word8Array.array 1 (Word8.fromInt 2))) (ByteArray.empty 0)
		end

		fun test_write_data_stream_can_connect_to_acceptor () =
		let
			fun validate state = 
				Assert.assertTrue state

			val address = InAddr.from_string "127.0.0.1"
			val port = 12345
			val sockaddr = SockAddrIn address port

			val inbuf = ByteArray.empty 2
			val _ = Marshalling.n2w2 port inbuf 0
			val outbuf = ByteArray.empty 0

			val _ = #(test_data_stream_can_connect_to_acceptor) (ByteArray.to_string inbuf) outbuf

			fun on_connect state fd = (
				(* We successfully connected to a socket.
			 	 * As it is exactly what we would like to test,
			   	 * just terminate the reactor. *)
				(True, Some ExitRun)
			)

			(* Indicates whether on_connect callback function has been called. *)
			val state = False
			val setup_req = AddWriteDataStream "write_data_stream" 
						(Some (ConnectHandler on_connect)) (ErrHandler on_error) 
						0 0 None (add_data_stream_callback sockaddr) error_callback
			val r = Option.valOf (Reactor.init state logger setup_req)
		in
			Reactor.run r;
			validate (Reactor.get_state r);
			#(reactor_acceptor_tests_sleep) 
				(ByteArray.to_string (Word8Array.array 1 (Word8.fromInt 2))) (ByteArray.empty 0)
		end

		fun test_read_write_data_stream_can_connect_to_acceptor () =
		let
			fun validate state = 
				Assert.assertTrue state

			val address = InAddr.from_string "127.0.0.1"
			val port = 12345
			val sockaddr = SockAddrIn address port

			val inbuf = ByteArray.empty 2
			val _ = Marshalling.n2w2 port inbuf 0
			val outbuf = ByteArray.empty 0

			val _ = #(test_data_stream_can_connect_to_acceptor) (ByteArray.to_string inbuf) outbuf

			fun on_read state fd buff = (
				(* Read handler should not be called, as the connect handler
				 * should be called first, that exits the reactor.
				 *)
				Assert.fail "Read handler called"
			)
			
			fun on_connect state fd = (
				(* We successfully connected to a socket.
				 * As it is exactly what we would like to test,
				 * just terminate the reactor. *)
				(True, Some ExitRun)
			)

			(* Indicates whether on_connect callback function has been called. *)
			val state = False
			val setup_req = AddReadWriteDataStream "read_write_data_stream" (ReadHandler on_read) 
						(Some (ConnectHandler on_connect)) (ErrHandler on_error) 
						100 100 0 0 None (add_data_stream_callback sockaddr) error_callback
			val r = Option.valOf (Reactor.init state logger setup_req)
		in
			Reactor.run r;
			validate (Reactor.get_state r);
			#(reactor_acceptor_tests_sleep) 
				(ByteArray.to_string (Word8Array.array 1 (Word8.fromInt 2))) (ByteArray.empty 0)
		end

	end


	local
		fun add_data_stream_callback sockaddr state fd =
			let
				fun connect_callback state = (state, None)
				
				val (SockAddrIn _ port) = sockaddr
				val inbuf = ByteArray.empty 6
				val _ = Marshalling.n2w2 port inbuf 0
				val _ = MarshallingHelp.n2w4 fd inbuf 2
				val outbuf = ByteArray.empty 0
			in
				#(test_data_stream_can_read_incoming_message) (ByteArray.to_string inbuf) outbuf;
				(state, Some (Connect fd sockaddr connect_callback error_callback))
			end
	in
		fun test_read_data_stream_can_read_incoming_message () =
		let
			val text = "TESTING DATA. SHOULD BE READ IN NON-BLOCKING MODE"
			fun validate state =
				let
					val was_read = state
				in
					Assert.assertEqualString text was_read
				end

			val address = InAddr.from_string "127.0.0.1"
			val port = 12345
			val sockaddr = SockAddrIn address port

			fun on_read state fd buff = 
			let
				val size = IOBuffer.size buff
				val msg = IOBuffer.read buff size
				val new_state = state ^ msg
			in
				if 
					not (String.isPrefix new_state text)
				then
					Assert.fail "Invalid message has been read"
				else (
					if String.compare new_state text = Equal
					then (
						validate new_state;
						(new_state, size, Some ExitRun)
					)
					else (new_state, size, None)
				)
			end

			(* Keeps track of what has been read at the moment. *)
			val state = ""
			val setup_req = AddReadDataStream "read_data_stream" (ReadHandler on_read) 
						None (ErrHandler on_error) 100 100 None 
						(add_data_stream_callback sockaddr) error_callback
			val r = Option.valOf (Reactor.init state logger setup_req)
		in
			Reactor.run r;
			#(reactor_acceptor_tests_sleep) 
				(ByteArray.to_string (Word8Array.array 1 (Word8.fromInt 2))) (ByteArray.empty 0)
		end

		fun test_read_write_data_stream_can_read_incoming_message () =
		let
			val text = "TESTING DATA. SHOULD BE READ IN NON-BLOCKING MODE"
			fun validate state =
				let
					val was_read = state
				in
					Assert.assertEqualString text was_read
				end

			val address = InAddr.from_string "127.0.0.1"
			val port = 12345
			val sockaddr = SockAddrIn address port

			fun on_read state fd buff = 
			let
				val size = IOBuffer.size buff
				val msg = IOBuffer.read buff size
				val new_state = state ^ msg
			in
				if 
					not (String.isPrefix new_state text)
				then
					Assert.fail "Invalid message has been read"
				else (
					if String.compare new_state text = Equal
					then (
						validate new_state;
						(new_state, size, Some ExitRun)
					)
					else (new_state, size, None)
				)
			end

			(* Keeps track of what has been read at the moment. *)
			val state = ""
			val setup_req = AddReadWriteDataStream "read_write_data_stream" (ReadHandler on_read) 
						None (ErrHandler on_error) 100 100 0 0 None 
						(add_data_stream_callback sockaddr) error_callback
			val r = Option.valOf (Reactor.init state logger setup_req)
		in
			Reactor.run r;
			#(reactor_acceptor_tests_sleep) 
				(ByteArray.to_string (Word8Array.array 1 (Word8.fromInt 2))) (ByteArray.empty 0)
		end
	end

	fun test_read_write_data_stream_can_write_and_read () =
	let
		val text = "TESTING DATA. SHOULD BE READ IN NON-BLOCKING MODE"
		fun validate state = 
			let
				val was_read = snd (snd state)
			in
				Assert.assertEqualString text was_read
			end

		val address = InAddr.from_string "127.0.0.1"
		val port = 12345
		val sockaddr = SockAddrIn address port

		val inbuf = ByteArray.empty 2
		val _ = Marshalling.n2w2 port inbuf 0
		val outbuf = ByteArray.empty 0
		val _ = #(test_read_write_data_stream_can_write_and_read) (ByteArray.to_string inbuf) outbuf
	
		fun on_error state fd errno =
			Assert.fail "Error occured"
		fun on_read state fd buff =
		let
			val size = IOBuffer.size buff
			val msg = IOBuffer.read buff size

			val was_read = snd (snd state) ^ msg
			val new_state = (fst state, (fst (snd state), was_read))
		in
			if 
				not (String.isPrefix was_read text)
			then 
				Assert.fail "Invalid message has been read"
			else (
				if String.compare was_read text = Equal
				then (
					validate new_state;
					(new_state, size, Some ExitRun)
				)
				else 
					(new_state, size, None)
			)
		end
		fun on_timer state fd n_exp =
		let
			fun write_callback state = (state, None)

			val n_send = fst (snd state)
			val n_to_send = min 8 (String.size text - n_send)
			
			val data = ByteArray.from_string (String.substring text n_send n_to_send)

			val new_state = (fst state, (n_send + n_to_send, snd (snd state)))

			val write_fd = fst state
			val write_request = Write write_fd data write_callback error_callback
		in
			if n_to_send = 0
			then 
				(* all data has been sent and we can safely
				* remove the descriptor from the reactor. 
				* TODO: return Remove request when it would be added. *)
				(state, Some (Remove write_fd (fn s => (s, Some (Remove fd (fn s => (s, None)))))))
			else
				(new_state, Some write_request)
		end

		fun add_data_stream_callback sockaddr state fd =
			let
				fun connect_callback state = 
					let
						fun add_timer_callback state fd = (state, None)
						val add_timer_request = 
							AddTimer "timer" 1500000 1000000 (TimerHandler on_timer)
								(ErrHandler on_error) add_timer_callback error_callback
					in
					  	(state, Some add_timer_request)
					end
			in
				((fd, snd state), Some (Connect fd sockaddr connect_callback error_callback))
			end

		(* Store the file descriptor corresponding to connection socket,
			* and keeps track of number of bytes that have been sent 
			* and of what has been read at the moment. *)
		val state = (~1, (0, ""))
		val setup_req = AddReadWriteDataStream "read_write_data_stream" (ReadHandler on_read) 
					None (ErrHandler on_error) 100 100 0 0 None 
					(add_data_stream_callback sockaddr) error_callback
		val r = Option.valOf (Reactor.init state logger setup_req)
	in
		Reactor.run r;
		#(reactor_acceptor_tests_sleep) 
			(ByteArray.to_string (Word8Array.array 1 (Word8.fromInt 6))) (ByteArray.empty 0)
	end

    (****************************************)        
    

    fun suite () =
		Test.labelTests
		[
			("test reactor can create read data stream and connect to acceptor", test_read_data_stream_can_connect_to_acceptor),
			("test reactor can create write data stream and connect to acceptor", test_write_data_stream_can_connect_to_acceptor),
			("test reactor can create read-write data stream and connect to acceptor", test_read_write_data_stream_can_connect_to_acceptor),

			("test created read data stream can read incoming message", test_read_data_stream_can_read_incoming_message),
			("test created read-write data stream can read incoming message", test_read_write_data_stream_can_read_incoming_message),
			
			("test created read-write data stream can write and read simultaneously", test_read_write_data_stream_can_write_and_read)
		]
end;

TextUITestRunner.runTest
	(TextUITestRunner.Output TextIO.stdOut)
	"ReactorDataStreamTests"
    (ReactorDataStreamTests.suite ());

