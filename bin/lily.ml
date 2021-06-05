open LilyCommand.Command

type command_kind = 
    | CommandKindBuild
    | CommandKindCompile
    | CommandKindError
    | CommandKindHelp
    | CommandKindInit
    | CommandKindNew
    | CommandKindRun
    | CommandKindTo
    | CommandKindVersion

let arg_to_command_kind arg = 
    match arg with
    | "build" -> CommandKindBuild
    | "compile" -> CommandKindCompile
    | "-h" | "--help" | "help" -> CommandKindHelp
    | "init" -> CommandKindInit
    | "new" -> CommandKindNew
    | "run" -> CommandKindRun
    | "to" -> CommandKindTo
    | "-v" | "--version" | "version" -> CommandKindVersion
    | _ -> CommandKindError

let _ = 
    match Array.length Sys.argv with
    | 1 -> Printf.printf "help"
    | _ -> match arg_to_command_kind Sys.argv.(1) with
           | CommandKindBuild -> Printf.printf "build"
           | CommandKindCompile -> Printf.printf "compile"
           | CommandKindError -> Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: unexpected command: \'%s\'\027\n" Sys.argv.(1)
           | CommandKindHelp -> LilyCommand.lily_help ()
           | CommandKindInit -> Printf.printf "init"
           | CommandKindNew -> Printf.printf "new"
           | CommandKindRun -> (
               match Array.length Sys.argv with
               | 2 -> Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: file doesn\'t specified\027\n"
               | _ -> LilyCommand.lily_run (Sys.argv.(2)) ())
           | CommandKindTo -> Printf.printf "to"
           | CommandKindVersion -> LilyCommand.lily_version ()
