module LilyCommand : sig
    val lily_help : unit -> unit
    val lily_run : string -> unit -> unit
    val lily_version : unit -> unit
end

module LilyBuildCommand : sig
    val build_help_op : unit -> unit
end

module LilyCompileCommand : sig
    val compile_help_op : unit -> unit
end

module LilyInitCommand : sig
    val init_help_op : unit -> unit
end

module LilyNewCommand : sig
    val new_help_op : unit -> unit
end

module LilyRunCommand : sig
    val run_help_op : unit -> unit
end

module LilyTestCommand : sig
    val test_help_op : unit -> unit
end

module LilyToCommand : sig
    val to_help_op : unit -> unit
end
