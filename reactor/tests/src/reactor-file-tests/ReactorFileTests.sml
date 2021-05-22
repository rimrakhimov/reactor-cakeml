structure ReactorFileTests =
struct
    val logger = StdOutLogger.init LoggerLevel.Info

    fun add_read_file_callback state = 
        (state, None)
    fun error_callback state errno = 
        Assert.fail "Error occured"

    fun test_file_read_non_blocking () =
	let
		val text = "TESTING DATA. SHOULD BE READ IN NON-BLOCKING MODE"
        fun validate state =
            let
                val was_read = state
            in
                Assert.assertEqualString text was_read
            end

		val outbuf = ByteArray.empty 4
		val _ = #(test_file_read_non_blocking) "" outbuf
		val read_fd = MarshallingHelp.w42n outbuf 0

		fun on_error state fd =
			Assert.fail "Error occured"
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
				if String.compare state text = Equal
				then (
                    validate new_state;
                    (new_state, size, Some ExitRun)
                )
				else (new_state, size, None)
			)
		end

        (* Keeps track of what has been read at the moment. *)
        val state = ""
        val setup_req = AddReadFile "read_end_of_pipe" read_fd 
                    (ReadHandler on_read) (ErrHandler on_error) (64 * 1024) (32 * 1024) 
                    add_read_file_callback error_callback
        val r = Option.valOf (Reactor.init state logger setup_req)
	in
        Reactor.run r
	end

	(* fun test_file_read_and_write_in_two_pipes_non_blocking () =
	let
		val text = "TESTING DATA. SHOULD BE READ IN NON-BLOCKING MODE"
		val (send_n : int ref) = Ref 0
		val (was_read : string ref) = Ref ""

		val outbuf = Word8Array.array 16 (Word8.fromInt 0)
		val _ = #(test_file_read_and_write_in_two_pipes_non_blocking) "" outbuf
		
		val write_fd = BigEndian.byte8int8 (ByteArray.from_string (Word8Array.substring outbuf 0 8))
		val read_fd = BigEndian.byte8int8 (ByteArray.from_string (Word8Array.substring outbuf 8 8))

		fun on_error reactor fd err_code msg =
			Assert.fail "Error occured"
		fun on_read reactor fd buff =
		let
			val size = IOBuffer.size buff
			val data = IOBuffer.read buff size
		in
			was_read := (!was_read) ^ (ByteArray.to_string data);
			if 
				not (String.isPrefix (!was_read) text)
			then 
				Assert.fail "Invalid message has been read"
			else (
				if String.compare (!was_read) text = Equal
				then Reactor.exit_run reactor
				else (size, reactor)
			)
		end
		fun on_timer reactor fd =
		let
			val size_to_send = min 8 (String.size text - (!send_n))
			val (data : byte_array) = ByteArray.from_string (String.substring text (!send_n) size_to_send)
			
			val reactor' = Reactor.write reactor write_fd data
		in
			send_n := (!send_n) + size_to_send;
			if 
				(!send_n) = String.size text
			then
				Reactor.remove_from_epoll reactor' fd
			else
				reactor'
		end

		val logger = StdOutLogger.init LoggerLevel.Off
		val r = Reactor.create 2 logger

		val write_r = Reactor.add_file r "write_end_of_pipe" write_fd None on_error 0 0 (64 * 1000) (32 * 1000)
		val read_r = Reactor.add_file write_r "read_end_of_pipe" read_fd (Some on_read) on_error (64 * 1000) (32 * 1000) 0 0
		val (_, timer_r) = Reactor.add_timer read_r "send_timer" 3000000 2000000 on_timer on_error

		val final_r = Reactor.run timer_r
	in
		Assert.assertEqualString text (!was_read)
	end *)


    (****************************************)        
    

    fun suite () =
		Test.labelTests
		[
			("test file read opened in non-blocking mode", test_file_read_non_blocking)
			(* ("test file read and write with two pipes opened in non-blocking mode", test_file_read_and_write_in_two_pipes_non_blocking) *)
		]
end;

TextUITestRunner.runTest
	(TextUITestRunner.Output TextIO.stdOut)
	"ReactorFileTests"
    (ReactorFileTests.suite ());

