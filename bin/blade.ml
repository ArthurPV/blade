open Blade_command.Command

let _ = 
    match Array.length Sys.argv with
    | 1 -> Printf.printf "help"
    | _ -> match Sys.argv.(1) with
           | "build" -> Printf.printf "build"
           | "compile" -> Printf.printf "compile"
           | "-h" | "--help" | "help" -> Printf.printf "help"
           | "init" -> Printf.printf "init"
           | "new" -> Printf.printf "new"
           | "run" -> Printf.printf "run"
           | "to" -> Printf.printf "to"
           | "-v" | "--version" | "version" -> BladeCommand.blade_version
           | _ -> Printf.printf "error"
