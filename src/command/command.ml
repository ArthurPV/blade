open Help
open Run
open Version

module KwhaleCommand = struct
    let kwhale_help () = Printf.printf "%s" kwhale_help

    let kwhale_run filename () = run filename

    let kwhale_version () = Printf.printf "blade v%s\n" kwhale_version
end
