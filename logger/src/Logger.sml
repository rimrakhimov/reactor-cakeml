(**
 *  Type keeps internal logger representation.
 *  @param outstream `TextIO.outstream`: the stream where logs should be written to.
 *  @param log_lvl `LoggerLevel.level`: the minimal level of messages that should be logged.
        If a message has a lower level it will be ignored.
 *)
datatype logger = Logger TextIO.outstream LoggerLevel.level;

(**
 *  Structure defines getters and setters for variables of internal logger representation.
 *  Should be used only from Logger structure and should not be used externally.
 *)
structure LoggerPrivate =
struct
    fun create (outstream : TextIO.outstream) (log_lvl : LoggerLevel.level) =
        Logger outstream log_lvl

    fun get_outstream (Logger outstream _) = outstream
    fun get_log_lvl (Logger _ log_lvl) = log_lvl
    
    fun set_outstream (Logger _ log_lvl) outstream =
        Logger outstream log_lvl
    fun set_log_lvl (Logger outstream _) log_lvl = 
        Logger outstream log_lvl
end ;

(**
 *  Structure defines functions to be used with logger to save logs.
 *)
structure Logger =
struct
    fun create (outstream : TextIO.outstream) (log_lvl : LoggerLevel.level) =
        LoggerPrivate.create outstream log_lvl

    fun level logger = LoggerPrivate.get_log_lvl logger

    fun set_level logger (level : LoggerLevel.level) = 
        LoggerPrivate.set_log_lvl logger level

    fun should_log logger (msg_lvl : LoggerLevel.level) =
        LoggerLevel.geq msg_lvl (LoggerPrivate.get_log_lvl logger)

    (**
     *  Internal function that outputs given message into outstream 
     *   if level of the message greater or equal than required by the logger.
     *
     *  NOTICE: Should not be used externally as the message is not formatted properly in the function.
     *   Use functions below to log a message at certain level.
     *)
    fun log logger (lvl : LoggerLevel.level) (msg : string) =
        if 
            should_log logger lvl andalso not (LoggerLevel.equal lvl LoggerLevel.Off)
        then 
            TextIO.output (LoggerPrivate.get_outstream logger) msg
        else 
            ()

    (**
     *  Functions to log messages of specific level of importance.
     *)
    local
        fun prepare_log (lvl : LoggerLevel.level) (msg : string) = 
            LoggerLevel.to_string lvl ^ ": " ^ msg ^ "\n\n"
    in
        fun trace logger (msg : string) =
            log logger LoggerLevel.Trace (prepare_log LoggerLevel.Trace msg)

        fun debug logger (msg : string) =
            log logger LoggerLevel.Debug (prepare_log LoggerLevel.Debug msg)

        fun info logger (msg : string) =
            log logger LoggerLevel.Info (prepare_log LoggerLevel.Info msg)

        fun warn logger (msg : string) =
            log logger LoggerLevel.Warn (prepare_log LoggerLevel.Warn msg)

        fun error logger (msg : string) =
            log logger LoggerLevel.Error (prepare_log LoggerLevel.Error msg)

        fun critical logger (msg : string) =
            log logger LoggerLevel.Critical (prepare_log LoggerLevel.Critical msg)
    end
end ;
