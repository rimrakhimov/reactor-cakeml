structure ReactorTimerTests =
struct
    val eps = 2000

    val logger = StdOutLogger.init LoggerLevel.Off

    fun add_timer_callback state fd = (state, None)
    fun set_timer_callback state = (state, None)
    fun error_callback state errno = 
        Assert.fail "Error occured"

    fun test_timer_fires_after_creation () =
        let
            val initial = 2000000
            val period = 0
            fun validate state =
                let
                    val was_created_at = fst state
                    val was_fired_at = snd state
                    val diff = was_fired_at - was_created_at
                in
                    Assert.assert 
                        ("Timer has been fired after invalid period of time. " ^ 
                         "Was created at: " ^ Int.toString was_created_at ^ 
                         ", was fired at: " ^ Int.toString was_fired_at ^
                         ". The absolute difference with expected: " ^ Int.toString (abs_diff diff initial))
                        (abs_diff diff initial < eps)
                end

            fun on_error state fd errno =
                Assert.fail "Error occured"
            fun on_timer state fd n_exp = (
                validate (fst state, Time.current_mcsec ());
                (state, Some ExitRun)
            )

            (* The state keeps timestamps then timer has been created and when it has fired at. *)
            val (state : int * int) = (Time.current_mcsec (), ~1)
            val setup_req = AddTimer "timer" initial period (TimerHandler on_timer) 
                    (ErrHandler on_error) add_timer_callback error_callback
            val r = Option.valOf (Reactor.init state logger setup_req)
        in
            Reactor.run r
        end

    fun test_timer_fires_after_specified_period_after_creation () =
        let
            val initial = 500000
            val period = 1500000
            fun validate state = 
                let
                    val was_first_fired_at = fst state
                    val was_second_fired_at = snd state
                    val diff = was_second_fired_at - was_first_fired_at
                in
                    Assert.assert 
                        ("Timer has been fired after invalid period of time. " ^ 
                         "Was first fired at: " ^ Int.toString was_first_fired_at ^ 
                         ", was second fired at: " ^ Int.toString was_second_fired_at ^
                         ". The absolute difference with expected: " ^ Int.toString (abs_diff diff period))
                        (abs_diff diff period < eps)
                end

            fun on_error state fd errno =
                Assert.fail "Error occured"
            fun on_timer state fd n_exp = 
                if 
                    fst state < 0
                then
                    ((Time.current_mcsec (), ~1), None)
                else (
                    validate (fst state, Time.current_mcsec ());
                    (state, Some ExitRun)
                )
                
            (* The state keeps timestamps timer has been fired first and second times. *)
            val state = (~1, ~1)
            val setup_req = AddTimer "timer" initial period (TimerHandler on_timer) 
                    (ErrHandler on_error) add_timer_callback error_callback
            val r = Option.valOf (Reactor.init state logger setup_req)
        in
            Reactor.run r
        end

    (*****************************************************************************************)

    (*
     *  We have a buffer initiated on `add_timer` to keep maximum 32 expirations.
     *  Thus, here we would like to ckeck that more than 32 expirations on the same timer
     *  works properly.
     *)
    fun test_timer_fires_128_times () =
        let
            val initial = 10000
            val period = 10000

            val limit = 128
            fun validate state = 
                Assert.assertEqualInt limit state

            fun on_error state fd errno =
                Assert.fail "Error occurred"
            fun on_timer state fd n_exp =
                if state < limit
                then (state + 1, None)
                else (
                    validate state;
                    (state, Some ExitRun)
                )

            (* The state keeps track of how many times a timer has been expired. *)
            val state = 0
            val r = Reactor.init state logger
            val setup_req = AddTimer "timer" initial period (TimerHandler on_timer) 
                    (ErrHandler on_error) add_timer_callback error_callback
            val r = Option.valOf (Reactor.init state logger setup_req)
        in
            Reactor.run r
        end
        

    (*****************************************************************************************)

    fun test_timer_fires_after_setting () =
        let
            val initial = 2000000
            val period = 0
            fun validate state =
                let
                    val was_set_at = fst state
                    val was_fired_at = snd state
                    val diff = was_fired_at - was_set_at
                in
                    Assert.assert 
                        ("Timer has been fired after invalid period of time. " ^ 
                         "Was set at: " ^ Int.toString was_set_at ^ 
                         ", was fired at: " ^ Int.toString was_fired_at ^
                         ". The absolute difference with expected: " ^ Int.toString (abs_diff diff initial))
                        (abs_diff diff initial < eps)
                end

            fun on_error state fd errno =
                Assert.fail "Error occured"
            fun on_timer state fd n_exp =
                let
                    val was_set_at = fst state
                in
                    if was_set_at < 0
                    then (
                        (Time.current_mcsec (), ~1), 
                        Some (SetTimer fd initial period set_timer_callback error_callback)
                    )
                    else (
                        validate (was_set_at, Time.current_mcsec ());
                        (state, Some ExitRun)
                    )
                end

            (* Keeps track of timestamps when the timer was set and when it was expired. *)
            val state = (~1, ~1)
            val setup_req = AddTimer "timer" 100 100 (TimerHandler on_timer) 
                    (ErrHandler on_error) add_timer_callback error_callback
            val r = Option.valOf (Reactor.init state logger setup_req)
        in
            Reactor.run r
        end

    fun test_timer_fires_after_specified_period_after_setting () =
        let
            val initial = 500000
            val period = 1500000
            fun validate state =
                let
                    val (was_first_fired_at, was_second_fired_at) = state
                    val diff = was_second_fired_at - was_first_fired_at
                in
                    Assert.assert 
                        ("Timer has been fired after invalid period of time. " ^ 
                         "Was first fired at: " ^ Int.toString was_first_fired_at ^ 
                         ", was second fired at: " ^ Int.toString was_second_fired_at ^
                         ". The absolute difference with expected: " ^ Int.toString (abs_diff diff period))
                        (abs_diff diff period < eps)
                end

            fun add_timer_callback state fd =
                (state, Some (SetTimer fd initial period set_timer_callback error_callback))

            fun on_error state fd errno =
                Assert.fail "Error occured"
            fun on_timer state fd n_exp = 
                if 
                    fst state < 0
                then
                    ((Time.current_mcsec (), ~1), None)
                else (
                    validate (fst state, Time.current_mcsec ());
                    (state, Some ExitRun)
                )

            (* Keeps track of timestamps when the timer expired 
             * the first time and when it expired the second 
             * time after setting. *)
            val state = (~1, ~1)
            val setup_req = AddTimer "timer" 100 100 (TimerHandler on_timer)
                    (ErrHandler on_error) add_timer_callback error_callback
            val r = Option.valOf (Reactor.init state logger setup_req)
        in
            Reactor.run r
        end

    
    (****************************************)        
    

    fun suite () =
        Test.labelTests
        [
            ("test timer fires after creation", test_timer_fires_after_creation),
            ("test timer fires after specified period after creation", test_timer_fires_after_specified_period_after_creation),

            ("test timer fires more than 32 times", test_timer_fires_128_times),

            ("test_timer_fires_after_setting", test_timer_fires_after_setting),
            ("test timer fires after specified period after setting", test_timer_fires_after_specified_period_after_setting)
        ]
end

val _ = 
    TextUITestRunner.runTest
        (TextUITestRunner.Output TextIO.stdOut)
        "ReactorTimerTests"
        (ReactorTimerTests.suite ())
