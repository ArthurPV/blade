open Help
open Run
open Version

module BladeCommand = struct
    let blade_help () = Printf.printf "%s" blade_help

    let blade_run filename () = run filename

    let blade_version () = Printf.printf "blade v%s\n" blade_version
end
