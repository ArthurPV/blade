open Help
open Run
open Version

external get_os : unit -> string = "get_os"
external get_arch : unit -> string = "get_arch"

module LilyCommand = struct
    let lily_help () = Printf.printf "%s" lily_h

    let lily_run filename () = run filename

    let lily_version () = 
        Printf.printf "lily v%s on %s/%s\n" lily_v (get_os ()) (get_arch ())
end
