(**
 *  Structure defines a logger that outputs logs into stdout.
 *)
structure StdOutLogger =
struct
    fun init (lvl : LoggerLevel.level) =
        Logger.create TextIO.stdOut lvl
end ;

(**
 *  Structure defines a logger that outputs logs into a file.
 *)
structure FileLogger =
struct
    (**
     *  Initializes a logger that writes logs into a file located at \a filepath.
     *
     *  In case if file at specified path already exists, the function truncates 
     *  all data and starts to write from the beginning of the file.
     *)
    fun init (filepath : string) (lvl : LoggerLevel.level) =
        let
            val outstream = TextIO.openOut filepath
        in
            Logger.create outstream lvl
        end

    (**
     *  Initializes a logger that writes logs into a file located at \a filepath.
     *  
     *  The only difference with `init` function is that in case if file at specified path already exists, 
     *  it appends the suffix '.#n' to the name of the file, where #n starts with 1 and is incrementing 
     *  until the file with specified filename and suffix does not exist.
     *)
    fun init_enum (filepath : string) (lvl : LoggerLevel.level) =
        let
            fun exists filepath =
                (**
                 *  There is no special functions to check whether file exists in CakeMl.
                 *  Thus, we just try to open the file in reading mode, and if operation
                 *  succeeds the file should exist at the given path. 
                 *)
                (TextIO.closeIn (TextIO.openIn filepath); True) 
                handle TextIO.BadFileName => False

            fun get_non_existent_filepath (filepath : string) (index : int) =
                let
                    val filepath_with_suffix = filepath ^ "." ^ Int.toString index
                in
                    if 
                        exists filepath_with_suffix
                    then 
                        get_non_existent_filepath filepath (index + 1)
                    else 
                        filepath_with_suffix
                end

            val non_existent_filepath = 
                if 
                    exists filepath 
                then 
                    get_non_existent_filepath filepath 1
                else 
                    filepath

            val outstream = TextIO.openOut non_existent_filepath
        in
            Logger.create outstream lvl
        end

    (**
     *  Closes the outstream that a logger uses.
     *
     *  Should be called when the logger is not further needed
     *  and the outstream can be freed.
     *)
    fun close logger =
        let
            val outstream = LoggerPrivate.get_outstream logger
        in
            TextIO.closeOut outstream
        end
end ;
