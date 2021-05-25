structure ReactorAcceptorTests =
struct
    val logger = StdOutLogger.init LoggerLevel.Error

    fun error_callback state errno = 
        Assert.fail "Error occured"

    fun test_can_accept_connection () =
	let
		fun validate state =
			(* The state before it was increased *)
			Assert.assertEqualInt 0 state

		val address = InAddr.from_string "127.0.0.1"
        val port = 12345

        val inbuf = ByteArray.empty 2
        val _ = Marshalling.n2w2 port inbuf 0
		val outbuf = ByteArray.empty 0

		fun add_acceptor_callback state fd = (
			#(test_can_accept_connection) (ByteArray.to_string inbuf) outbuf;
        	(state, None)
		)

		fun on_error state fd errno =
			Assert.fail "Error occured"
		fun on_accept state acceptor_fd client_fd client_addr = (
			validate state;
			(* We successfully accepted a new connection. 
			 * As it is exactly what we would like to test,
			 * just terminate the reactor. *)
			(state + 1, Some ExitRun)
		)

		(* Indicates how many times on_accept callback function has been called. *)
        val state = 0
        val setup_req = AddAcceptor "acceptor" (AcceptHandler on_accept)
                    (ErrHandler on_error) (SockAddrIn address port) 100
                    add_acceptor_callback error_callback
        val r = Option.valOf (Reactor.init state logger setup_req)
	in
		Reactor.run r;
		#(reactor_acceptor_tests_sleep) 
			(ByteArray.to_string (Word8Array.array 1 (Word8.fromInt 2))) (ByteArray.empty 0)
	end

	fun test_can_accept_several_connections () =
	let
		val address = InAddr.from_string "127.0.0.1"
        val port = 12345
		val conn_num = 50
		fun validate state =
			(* The state before it was increased *)
			Assert.assertEqualInt (conn_num - 1) state

        val port_bytes = ByteArray.empty 2
        val _ = Marshalling.n2w2 port port_bytes 0
		val conn_num_bytes = MarshallingHelp.n2w4 conn_num

		val inbuf = ByteArray.concat_all [port_bytes, conn_num_bytes]
		val outbuf = ByteArray.empty 0

		fun add_acceptor_callback state fd = (
			#(test_can_accept_several_connections) (ByteArray.to_string inbuf) outbuf;
        	(state, None)
		)

		fun on_error state fd errno =
			Assert.fail "Error occured"
		fun on_accept state acceptor_fd client_fd client_addr = (
			if state < conn_num - 1
			then (state + 1, None)
			else (
				validate state;
				(state + 1, Some ExitRun)
			)
		)

		(* Indicates how many times on_accept callback function has been called. *)
        val state = 0
        val setup_req = AddAcceptor "acceptor" (AcceptHandler on_accept)
                    (ErrHandler on_error) (SockAddrIn address port) 100
                    add_acceptor_callback error_callback
        val r = Option.valOf (Reactor.init state logger setup_req)
	in
		Reactor.run r;
		#(reactor_acceptor_tests_sleep) 
			(ByteArray.to_string (Word8Array.array 1 (Word8.fromInt 2))) (ByteArray.empty 0)
	end


    (****************************************)        
    

    fun suite () =
		Test.labelTests
		[
			("test acceptor can accept a connection", test_can_accept_connection),
			("test acceptor can accept several connections", test_can_accept_several_connections)
		]
end;

TextUITestRunner.runTest
	(TextUITestRunner.Output TextIO.stdOut)
	"ReactorAcceptorTests"
    (ReactorAcceptorTests.suite ());

