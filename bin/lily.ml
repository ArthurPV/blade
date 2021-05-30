open LilyCommand.Command

let _ = 
    match Array.length Sys.argv with
    | 1 -> Printf.printf "help"
    | _ -> match Sys.argv.(1) with
           | "build" -> Printf.printf "build"
           | "compile" -> Printf.printf "compile"
           | "-h" | "--help" | "help" -> MainLilyCommand.lily_help ()
           | "init" -> Printf.printf "init"
           | "new" -> Printf.printf "new"
           | "run" -> (
               match Array.length Sys.argv with
               | 2 -> Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: file doesn\'t specified\027\n"
               | _ -> MainLilyCommand.lily_run (Sys.argv.(2)) ())
           | "to" -> Printf.printf "to"
           | "-v" | "--version" | "version" -> MainLilyCommand.lily_version ()
           | _ -> Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: unexpected command: \'%s\'\027\n" Sys.argv.(1)
