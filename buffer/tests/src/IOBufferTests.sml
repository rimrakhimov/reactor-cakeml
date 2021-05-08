structure IOBufferTests =
struct

    (****************************************)

    fun test_init_empty () =
        let
            val iob = IOBuffer.init 10 5
            val is_empty = IOBuffer.empty iob
        in
            Assert.assertTrue is_empty
        end

    fun test_init_max_size () =
        let
            val iob = IOBuffer.init 10 5
            val max_size = IOBuffer.max_size iob
        in
            Assert.assertEqualInt 10 max_size
        end

    fun test_init_size () =
        let
            val iob = IOBuffer.init 10 5
            val size = IOBuffer.size iob
        in
            Assert.assertEqualInt 0 size
        end

    fun test_init_capacity () =
        let
            val iob = IOBuffer.init 10 5
            val capacity = IOBuffer.capacity iob
        in
            Assert.assertEqualInt 10 capacity
        end

    fun test_init_wr_lwm () =
        let
            val iob = IOBuffer.init 10 5
            val wr_lwm = IOBuffer.wr_lwm iob
        in
            Assert.assertEqualInt 5 wr_lwm
        end

    (****************************************)

    fun test_write_and_size () =
        let
            val iob = IOBuffer.init 10 5
            val _ = IOBuffer.write iob (ByteArray.from_string "hello")
            
            val size = IOBuffer.size iob
        in
            Assert.assertEqualInt 5 size
        end
    
    fun test_write_and_capacity () =
        let
            val iob = IOBuffer.init 10 5
            val _ = IOBuffer.write iob (ByteArray.from_string "hello")
            
            val capacity = IOBuffer.capacity iob
        in
            Assert.assertEqualInt 5 capacity
        end

    fun test_write_and_read () =
        let
            val iob = IOBuffer.init 10 5
            val data = ByteArray.from_string "hello"
            val _ = IOBuffer.write iob data
            
            val expected = ByteArray.to_string data
            val read = IOBuffer.read iob 5
        in
            Assert.assertEqualString expected read
        end

    fun test_write_and_read_does_not_consume () =
        let
            val iob = IOBuffer.init 10 5
            val data = ByteArray.from_string "hello"
            val _ = IOBuffer.write iob data
            val _ = IOBuffer.read iob 5

            val expected = ByteArray.to_string data
            val read = IOBuffer.read iob 5
        in
            Assert.assertEqualString expected read
        end

    (****************************************)

    fun test_write_and_consume_empty () =
        let
            val iob = IOBuffer.init 10 5
            val data = ByteArray.from_string "hello"
            val _ = IOBuffer.write iob data
            val _ = IOBuffer.consume iob 5

            val is_empty = IOBuffer.empty iob
        in
            Assert.assertTrue is_empty
        end

    fun test_write_and_consume_size () =
        let
            val iob = IOBuffer.init 10 5
            val data = ByteArray.from_string "hello"
            val _ = IOBuffer.write iob data
            val _ = IOBuffer.consume iob 5

            val size = IOBuffer.size iob
        in
            Assert.assertEqualInt 0 size
        end

    fun test_write_and_consume_capacity () =
        let
            val iob = IOBuffer.init 10 5
            val data = ByteArray.from_string "hello"
            val _ = IOBuffer.write iob data
            val _ = IOBuffer.consume iob 5

            val capacity = IOBuffer.capacity iob
        in
            Assert.assertEqualInt 5 capacity
        end

    (****************************************)

    fun test_write_and_consume_and_crunch_capacity () =
        let
            val iob = IOBuffer.init 10 5
            val data = ByteArray.from_string "hellowor"
            val _ = IOBuffer.write iob data
            val _ = IOBuffer.consume_and_crunch iob 5

            val capacity = IOBuffer.capacity iob
        in
            Assert.assertEqualInt 7 capacity
        end

    fun test_write_and_consume_and_crunch_read () =
        let
            val iob = IOBuffer.init 10 5
            val data_to_read = ByteArray.from_string "hello"
            val data_to_keep = ByteArray.from_string "wor"
            val _ = IOBuffer.write iob (ByteArray.concat data_to_read data_to_keep)
            val _ = IOBuffer.consume_and_crunch iob 5

            val expected = ByteArray.to_string data_to_keep
            val read = IOBuffer.read iob 3
        in
            Assert.assertEqualString expected read
        end

    (****************************************)

    fun test_write_to_expand_max_size () =
        let
            val iob = IOBuffer.init 10 5
            val data = ByteArray.from_string "hello, world"
            val _ = IOBuffer.write iob data

            val max_size = IOBuffer.max_size iob
        in
            Assert.assertEqualInt 12 max_size
        end

    fun test_write_to_expand_max_size_while_wr_offset_shift () =
        let
            val iob = IOBuffer.init 10 5
            val data1 = ByteArray.from_string "hello"
            val _ = IOBuffer.write iob data1

            val _ = IOBuffer.consume_and_crunch iob 2
            
            val data2 = ByteArray.from_string ", world"
            val _ = IOBuffer.write iob data2

            val expected = "llo, world"
            val read = IOBuffer.read iob 10
        in
            Assert.assertEqualString expected read
        end

    (****************************************)        

    fun test_init_too_high_wr_lwm_throws () =
        (IOBuffer.init 10 15; Assert.fail "too high write lowermark should fail")
        handle IOBufferLowWatermarkTooHigh 10 15 => ()


    fun test_read_not_enough_throws () =
        let
            val iob = IOBuffer.init 10 5
            val _ = IOBuffer.write iob (ByteArray.from_string "hello")
        in
            (IOBuffer.read iob 6; Assert.fail "not enough data to read must fail")
            handle IOBufferNotEnoughData 5 6 => ()
        end

    fun test_consume_not_enough_throws () =
        let
            val iob = IOBuffer.init 10 5
            val _ = IOBuffer.write iob (ByteArray.from_string "hello")
        in
            (IOBuffer.consume iob 6; Assert.fail "not enough data to consume must fail")
            handle IOBufferNotEnoughData 5 6 => ()
        end

    fun test_consume_and_crunch_not_enough_throws () =
        let
            val iob = IOBuffer.init 10 5
            val _ = IOBuffer.write iob (ByteArray.from_string "hello")
        in
            (IOBuffer.consume_and_crunch iob 6; Assert.fail "not enough data to consume must fail")
            handle IOBufferNotEnoughData 5 6 => ()
        end

    (****************************************)        

    fun suite () =
		Test.labelTests
		[
			("init_empty", test_init_empty),
            ("init_max_size", test_init_max_size),
            ("init_size", test_init_size),
            ("init_capacity", test_init_capacity),
            ("init_wr_lwm", test_init_wr_lwm),

            ("write_and_size", test_write_and_size),
            ("write_and_capacity", test_write_and_capacity),
            ("write_and_read", test_write_and_read),
            ("write_and_read_does_not_consume", test_write_and_read_does_not_consume),

            ("write_and_consume_empty", test_write_and_consume_empty),
            ("write_and_consume_size", test_write_and_consume_size),
            ("write_and_consume_capacity", test_write_and_consume_capacity),

            ("write_and_consume_and_crunch_capacity", test_write_and_consume_and_crunch_capacity),
            ("write_and_consume_and_crunch_read", test_write_and_consume_and_crunch_read),

            ("write_to_expand_max_size", test_write_to_expand_max_size),
            ("test_write_to_expand_max_size_while_wr_offset_shift", test_write_to_expand_max_size_while_wr_offset_shift),

            ("init_too_high_wr_lwm_throws", test_init_too_high_wr_lwm_throws),
            ("read_not_enough_throws", test_read_not_enough_throws),
            ("consume_not_enough_throws", test_consume_not_enough_throws),
            ("consume_and_crunch_not_enough_throws", test_consume_and_crunch_not_enough_throws)
		]
end;

TextUITestRunner.runTest
	(TextUITestRunner.Output TextIO.stdOut)
	"IOBufferTests"
    (IOBufferTests.suite ());
