structure ReactorTimerTests =
struct
    val eps = 2000

    fun test_timer_fires_after_creation () =
       let
            val initial = 2000000

            fun on_error reactor fd =
                Assert.fail "Error occured"
            fun on_timer reactor fd n_exp = (
                Reactor.set_state reactor (Time.current_mcsec ());
                Reactor.exit_run reactor
            )

            val logger = StdOutLogger.init LoggerLevel.Off

            (* The state keeps the timestamp timer has been fired at. *)
            val (state : int) = ~1
            val r = Reactor.init state logger

            val was_created_at = Time.current_mcsec ()
            val _ = Reactor.add_timer r "timer" initial 0 on_timer on_error
            val _ = Reactor.run r

            val was_fired_at = Reactor.get_state r
            val diff = was_fired_at - was_created_at
       in
            Assert.assert 
                ("Timer has been fired after invalid period of time. " ^ 
                 "Was created at: " ^ Int.toString was_created_at ^ 
                 ", was fired at: " ^ Int.toString was_fired_at ^
                 ". The absolute difference with expected: " ^ Int.toString (abs_diff diff initial))
                (abs_diff diff initial < eps)
       end

    fun test_timer_fires_after_specified_period_after_creation () =
        let
            val initial = 500000
            val period = 1500000

            fun on_error reactor fd =
                Assert.fail "Error occured"
            fun on_timer reactor fd n_exp = 
                if 
                    (fst (Reactor.get_state reactor) < 0)
                then
                    Reactor.set_state reactor (Time.current_mcsec (), ~1)
                else
                let
                    val was_first_fired_at = fst (Reactor.get_state reactor)
                in
                    Reactor.set_state reactor (was_first_fired_at, Time.current_mcsec ());
                    Reactor.exit_run reactor
                end

            val logger = StdOutLogger.init LoggerLevel.Off
                
            (* The state keeps timestamps timer has been fired first and second times. *)
            val state = (~1, ~1)

            val r = Reactor.init state logger
            val _ = Reactor.add_timer r "timer" initial period on_timer on_error    
            val _ = Reactor.run r

            val (was_first_fired_at, was_second_fired_at) = Reactor.get_state r
            val diff = was_second_fired_at - was_first_fired_at
        in
            Assert.assert 
                ("Timer has been fired after invalid period of time. " ^ 
                 "Was first fired at: " ^ Int.toString was_first_fired_at ^ 
                 ", was second fired at: " ^ Int.toString was_second_fired_at ^
                 ". The absolute difference with expected: " ^ Int.toString (abs_diff diff period))
                (abs_diff diff period < eps)
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

            (* The state keeps track of how many times a timer has been expired. *)
            val state = 0

            val logger = StdOutLogger.init LoggerLevel.Off
            fun on_error reactor fd =
                Assert.fail "Error occurred"
            fun on_timer reactor fd n_exp =
                if Reactor.get_state reactor < limit
                then Reactor.set_state reactor (Reactor.get_state reactor + 1)
                else Reactor.exit_run reactor

            val r = Reactor.init state logger
            val _ = Reactor.add_timer r "timer" initial period on_timer on_error
            val _ = Reactor.run r

            val count = Reactor.get_state r
        in
            Assert.assertEqualInt limit count
        end
        

    (*****************************************************************************************)

    (* fun test_timer_fires_after_setting () =
        let
            val initial = 4000000
        
            val (was_set : bool ref) = Ref False
            val (was_set_at : int ref) = Ref 0
            val (was_fired_at : int ref) = Ref 0

            fun on_error reactor fd =
                Assert.fail "Error occured"
            fun on_timer reactor fd n_exp =
                if 
                    not (!was_set)
                then (
                    was_set := True;
                    was_set_at := Time.current_mcsec ();
                    Reactor.set_timer reactor fd initial 0
                ) else (
                    was_fired_at := Time.current_mcsec ();
                    Reactor.exit_run reactor
                )

            val logger = StdOutLogger.init LoggerLevel.Off
            val r = Reactor.create 2 logger

            val _ = Reactor.add_timer r "timer" 100 100 on_timer on_error

            val _ = Reactor.run r'

            val diff = (!was_fired_at) - (!was_set_at)
        in
            Assert.assert 
                ("Timer has been fired after invalid period of time. " ^ 
                 "Was set at: " ^ Int.toString (!was_set_at) ^ 
                 ", was fired at: " ^ Int.toString (!was_fired_at) ^
                 ". The absolute difference with expected: " ^ Int.toString (abs_diff diff initial))
                (abs_diff diff initial < eps)
        end *)

    (* fun test_timer_fires_after_specified_period_after_setting () =
       let
        val initial = 2000000
        val period = 5000000
        
        val (was_fired_once : bool ref) = Ref False
        val (was_set : bool ref) = Ref False

        val (was_set_at : int ref) = Ref 0
        val (was_fired_at : int ref) = Ref 0

               fun on_error reactor fd err_code msg =
                       Assert.fail "Error occured"
               fun on_timer reactor fd =
            if 
                not (!was_set)
            then (
                was_set := True;
                was_set_at := Time.current ();
                Reactor.set_timer reactor fd initial period
            ) else (
                if
                    (!was_fired_once)
                then (
                    was_fired_at := Time.current ();
                    Reactor.exit_run reactor
                ) else (
                    was_fired_once := True;
                    reactor
                )
            )

               val logger = StdOutLogger.init LoggerLevel.Off
               val r = Reactor.create 2 logger

               val (_, r') = Reactor.add_timer r "timer" 100 100 on_timer on_error
               
               val final_r = Reactor.run r'

        val diff = (!was_fired_at) - (!was_set_at)
       in
               Assert.assert 
            ("Timer has been fired after invalid period of time. " ^ 
             "Was set at: " ^ Int.toString (!was_set_at) ^ 
             ", was fired at: " ^ Int.toString (!was_fired_at) ^
             ". The absolute difference with expected: " ^ Int.toString (abs_diff diff (initial + period)))
            (abs_diff diff (initial + period) < eps)
       end *)

    
    (****************************************)        
    

    fun suite () =
        Test.labelTests
        [
            ("test timer fires after creation", test_timer_fires_after_creation),
            ("test timer fires after specified period after creation", test_timer_fires_after_specified_period_after_creation),
            ("test timer fires more than 32 times", test_timer_fires_128_times)

            (* ("test_timer_fires_after_setting", test_timer_fires_after_setting), *)
            (* ("test timer fires after specified period after setting", test_timer_fires_after_specified_period_after_setting) *)
        ]
end

val _ = 
    TextUITestRunner.runTest
        (TextUITestRunner.Output TextIO.stdOut)
        "ReactorTimerTests"
        (ReactorTimerTests.suite ())
