structure ReactorTimerTests =
struct
    val eps = 2000

    fun test_timer_fires_after_creation () =
       let
            val initial = 4000000
        
            val (was_fired_at : int ref) = Ref 0

            fun on_error reactor fd =
                Assert.fail "Error occured"
            fun on_timer reactor fd n_exp = (
                was_fired_at := Time.current_mcsec ();
                Reactor.exit_run reactor
            )

            val logger = StdOutLogger.init LoggerLevel.Off
            val r = Reactor.init 2 logger

            val was_created_at = Time.current_mcsec ()
            val _ = Reactor.add_timer r "timer" initial 0 on_timer on_error
            val _ = Reactor.run r

            val diff = (!was_fired_at) - was_created_at
       in
            Assert.assert 
                ("Timer has been fired after invalid period of time. " ^ 
                 "Was created at: " ^ Int.toString was_created_at ^ 
                 ", was fired at: " ^ Int.toString (!was_fired_at) ^
                 ". The absolute difference with expected: " ^ Int.toString (abs_diff diff initial))
                (abs_diff diff initial < eps)
       end

    fun test_timer_fires_after_specified_period_after_creation () =
        let
            val initial = 1000000
            val period = 3000000
        
            val (was_fired_once : bool ref) = Ref False
            val (was_fired_at : int ref) = Ref 0

            fun on_error reactor fd =
                Assert.fail "Error occured"
            fun on_timer reactor fd n_exp = 
                if 
                    (!was_fired_once)
                then (
                    was_fired_at := Time.current_mcsec ();
                    Reactor.exit_run reactor
                ) else (
                    was_fired_once := True
                )

                val logger = StdOutLogger.init LoggerLevel.Off
                val r = Reactor.init 2 logger

                val was_created_at = Time.current_mcsec ()
                val _ = Reactor.add_timer r "timer" initial period on_timer on_error
               
                val _ = Reactor.run r

                val diff = (!was_fired_at) - was_created_at
        in
            Assert.assert 
                ("Timer has been fired after invalid period of time. " ^ 
                 "Was created at: " ^ Int.toString was_created_at ^ 
                 ", was fired at: " ^ Int.toString (!was_fired_at) ^
                 ". The absolute difference with expected: " ^ Int.toString (abs_diff diff (initial + period)))
                (abs_diff diff (initial + period) < eps)
        end

    (*****************************************************************************************)

    (* fun test_timer_fires_128_times () =
         *)

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
            ("test timer fires after specified period after creation", test_timer_fires_after_specified_period_after_creation)

            (* ("test_timer_fires_after_setting", test_timer_fires_after_setting), *)
            (* ("test timer fires after specified period after setting", test_timer_fires_after_specified_period_after_setting) *)
        ]
end

val _ = 
    TextUITestRunner.runTest
        (TextUITestRunner.Output TextIO.stdOut)
        "ReactorTimerTests"
        (ReactorTimerTests.suite ())
