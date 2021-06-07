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

module LilyBuildCommand = struct 
    let build_help_op () = Printf.printf "%s" lily_build_h
end

module LilyCompileCommand = struct
    let compile_help_op () = Printf.printf "%s" lily_compile_h
end

module LilyInitCommand = struct
    let init_help_op () = Printf.printf "%s" lily_init_h
end

module LilyNewCommand = struct
    let new_help_op () = Printf.printf "%s" lily_new_h
end

module LilyRunCommand = struct
    let run_help_op () = Printf.printf "%s" lily_run_h
end

module LilyTestCommand = struct
    let test_help_op () = Printf.printf "%s" lily_test_h
end

module LilyToCommand = struct
    let to_help_op () = Printf.printf "%s" lily_to_h
end
