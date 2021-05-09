open Help
open Run
open Version

module MainLilyCommand = struct
    let lily_help () = Printf.printf "%s" lily_h

    let lily_run filename () = run filename

    let lily_version () = Printf.printf "blade v%s\n" lily_v
end
