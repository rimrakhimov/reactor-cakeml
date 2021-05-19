structure Time =
struct
    local
        val time_nsec = Timestamp.current ()

        (**
         *  Divide a value on (10^exp) and round the resultant
         *  value using the usual mathematical rounding rules.
         *)
        fun div_and_round value exp =
            let
                val divisor = funpow (Int.* 10) exp 1
                val rem = Int.mod value divisor
                val quot = Int.div value divisor
            in
                if rem < Int.div divisor 2
                then quot
                else quot + 1
            end
    in
        (**
         *  Returns current timestamp in nanoseconds.
         *
         *  @raises `FFIFailure` if syscall to get timestamp fails.
         *)
        fun current_nsec () = time_nsec

        (**
         *  Returns current timestamp in microseconds.
         *
         *  @raises `FFIFailure` if syscall to get timestamp fails.
         *)
        fun current_mcsec () = div_and_round time_nsec 3

        (**
         *  Returns current timestamp in milliseconds.
         *
         *  @raises `FFIFailure` if syscall to get timestamp fails.
         *)
        fun current_msec () = div_and_round time_nsec 6

        (**
         *  Returns current timestamp in seconds.
         *
         *  @raises `FFIFailure` if syscall to get timestamp fails.
         *)
        fun current_sec () = div_and_round time_nsec 9
    end
end
