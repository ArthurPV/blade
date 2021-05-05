module BladeCommand : sig
    val blade_help : unit -> unit
    val blade_run : string -> unit -> unit
    val blade_version : unit -> unit
end
