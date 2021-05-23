structure ReactorFileTests =
struct
    val logger = StdOutLogger.init LoggerLevel.Off

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

		fun add_read_file_callback state = 
        	(state, None)

		fun on_error state fd errno =
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
        val setup_req = AddReadFile "read_end_of_pipe" read_fd 
                    (ReadHandler on_read) (ErrHandler on_error) (64 * 1024) (32 * 1024) 
                    add_read_file_callback error_callback
        val r = Option.valOf (Reactor.init state logger setup_req)
	in
        Reactor.run r
	end

	fun test_file_read_and_write_in_two_pipes_non_blocking () =
	let
		val text = "TESTING DATA. SHOULD BE READ IN NON-BLOCKING MODE"
		fun validate state = 
			let
			  	val was_read = snd state
			in
			  	Assert.assertEqualString text was_read
			end

		val outbuf = ByteArray.empty 8
		val _ = #(test_file_read_and_write_in_two_pipes_non_blocking) "" outbuf
		
		val write_fd = MarshallingHelp.w42n outbuf 0
		val read_fd = MarshallingHelp.w42n outbuf 4

		fun on_error state fd errno =
			Assert.fail "Error occured"
		fun on_read state fd buff =
		let
			val size = IOBuffer.size buff
			val msg = IOBuffer.read buff size

			val was_read = snd state ^ msg
			val new_state = (fst state, was_read)
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
			val n_send = fst state
			val n_to_send = min 8 (String.size text - n_send)
			
			val data = ByteArray.from_string (String.substring text n_send n_to_send)

			val new_state = (n_send + n_to_send, snd state)

			val write_request = Write write_fd data error_callback
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

		fun add_write_file_callback state =
			let
				fun add_read_file_callback state =
					let
						fun add_timer_callback state fd = (state, None)
						val add_timer_request = 
							AddTimer "timer" 1500000 1000000 (TimerHandler on_timer)
								(ErrHandler on_error) add_timer_callback error_callback
					in
					  	(state, Some add_timer_request)
					end
				val add_read_file_request =
					AddReadFile "read_end_of_pipe" read_fd
					(ReadHandler on_read) (ErrHandler on_error) (64 * 1024) (32 * 1024) 
                    	add_read_file_callback error_callback
			in
				(state, Some add_read_file_request)
			end

		(* Keeps track of number of bytes that have been sent 
		 * and of what has been read at the moment. *)
        val state = (0, "")
		val setup_req = AddWriteFile "write_end_of_pipe" write_fd
					(ErrHandler on_error) (64 * 1024) (32 * 1024)
					add_write_file_callback error_callback
        val r = Option.valOf (Reactor.init state logger setup_req)
	in
		Reactor.run r
	end


    (****************************************)        
    

    fun suite () =
		Test.labelTests
		[
			("test file read opened in non-blocking mode", test_file_read_non_blocking),
			("test file read and write with two pipes opened in non-blocking mode", test_file_read_and_write_in_two_pipes_non_blocking)
		]
end;

TextUITestRunner.runTest
	(TextUITestRunner.Output TextIO.stdOut)
	"ReactorFileTests"
    (ReactorFileTests.suite ());

