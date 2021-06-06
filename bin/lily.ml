open LilyCommand.Command

type build_option = 
    | BuildOptionError
    | BuildOptionHelp

type compile_option = 
    | CompileOptionError
    | CompileOptionHelp

type init_option = 
    | InitOptionError
    | InitOptionHelp

type new_option = 
    | NewOptionError
    | NewOptionHelp

type run_option = 
    | RunOptionHelp
    | RunOptionFile of string
    | RunOptionFileError

type to_option = 
    | ToOptionError
    | ToOptionHelp

type command_kind = 
    | CommandKindBuild of build_option CCVector.vector
    | CommandKindCompile of compile_option CCVector.vector
    | CommandKindError
    | CommandKindHelp
    | CommandKindInit of init_option CCVector.vector
    | CommandKindNew of new_option CCVector.vector
    | CommandKindRun of run_option CCVector.vector
    | CommandKindTo of to_option CCVector.vector
    | CommandKindVersion

let arg_to_command_kind ?(arg=Sys.argv.(1)) () = 
    match arg with
    | "build" -> (
        let op = CCVector.create () in
        let rec loop count = 
            if Array.length Sys.argv > 1 && count < Array.length Sys.argv then
                match Sys.argv.(count) with
                | "-h" | "--help" -> (
                    loop (count+1);
                    CCVector.push op BuildOptionHelp)
                | _ -> (
                    CCVector.push op BuildOptionError;
                    ()) in
        loop (2);
        CommandKindBuild op)
    | "compile" -> (
        let op = CCVector.create () in
        let rec loop count = 
            if Array.length Sys.argv > 1 && count < Array.length Sys.argv then
                match Sys.argv.(count) with
                | "-h" | "--help" -> (
                    loop (count+1);
                    CCVector.push op CompileOptionHelp)
                | _ -> (
                    CCVector.push op CompileOptionError;
                    ()) in
        loop (2);
        CommandKindCompile op)
    | "-h" | "--help" | "help" -> CommandKindHelp
    | "init" -> (
        let op = CCVector.create () in
        let rec loop count = 
            if Array.length Sys.argv > 1 && count < Array.length Sys.argv then
                match Sys.argv.(count) with
                | "-h" | "--help" -> (
                    loop (count+1);
                    CCVector.push op InitOptionHelp)
                | _ -> (
                    CCVector.push op InitOptionError;
                    ()) in
        loop (2);
        CommandKindInit op)
    | "new" -> (
        let op = CCVector.create () in
        let rec loop count = 
            if Array.length Sys.argv > 1 && count < Array.length Sys.argv then
                match Sys.argv.(count) with
                | "-h" | "--help" -> (
                    loop (count+1);
                    CCVector.push op NewOptionHelp)
                | _ -> (
                    CCVector.push op NewOptionError;
                    ()) in
        loop (2);
        CommandKindNew op)
    | "run" -> (
        let op = CCVector.create () in
        let rec loop count = 
            if Array.length Sys.argv > 1 && count < Array.length Sys.argv then
                match Sys.argv.(count) with
                | "-h" | "--help" -> (
                    loop (count+1);
                    CCVector.push op RunOptionHelp)
                | _ -> (
                    loop (count+1);
                    if count < Array.length Sys.argv then CCVector.push op (RunOptionFile Sys.argv.(count))
                    else (
                        CCVector.push op RunOptionHelp;
                        ())) in
        loop (2);
        CommandKindRun op)
    | "to" -> (
        let op = CCVector.create () in
        let rec loop count = 
            if Array.length Sys.argv > 1 && count < Array.length Sys.argv then
                match Sys.argv.(count) with
                | "-h" | "--help" -> (
                    loop (count+1);
                    CCVector.push op ToOptionHelp)
                | _ -> (
                    CCVector.push op ToOptionError;
                    ()) in
        loop (2);
        CommandKindTo op)
    | "-v" | "--version" | "version" -> CommandKindVersion
    | _ -> CommandKindError

let _ = 
    match Array.length Sys.argv with
    | 1 -> LilyCommand.lily_help ()
    | _ -> match arg_to_command_kind () with
           | CommandKindBuild op -> (
               match Array.length Sys.argv with
               | 2 -> Printf.printf "help"
               | _ -> (
                   for i = 0 to (CCVector.length op)-1 do 
                       match CCVector.get op i with
                       | BuildOptionError -> (
                           Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: unexpected command: \'%s\'\027\n" Sys.argv.(i+2);
                           exit 1)
                       | BuildOptionHelp -> Printf.printf "help"
                   done;))
           | CommandKindCompile op -> (
               match Array.length Sys.argv with
               | 2 -> Printf.printf "help"
               | _ -> ( 
                   for i = 0 to (CCVector.length op)-1 do 
                       match CCVector.get op i with
                       | CompileOptionError -> (
                           Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: unexpected command: \'%s\'\027\n" Sys.argv.(i+2);
                           exit 1)
                       | CompileOptionHelp -> Printf.printf "help"
                   done;))
           | CommandKindError -> Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: unexpected command: \'%s\'\027\n" Sys.argv.(1)
           | CommandKindHelp -> LilyCommand.lily_help ()
           | CommandKindInit op -> (
               match Array.length Sys.argv with
               | 2 -> Printf.printf "help"
               | _ -> (
                   for i = 0 to (CCVector.length op)-1 do 
                       match CCVector.get op i with
                       | InitOptionError -> (
                           Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: unexpected command: \'%s\'\027\n" Sys.argv.(i+2);
                           exit 1)
                       | InitOptionHelp -> Printf.printf "help"
                   done;))
           | CommandKindNew op -> (
               match Array.length Sys.argv with
               | 2 -> Printf.printf "help"
               | _ -> (
                   for i = 0 to (CCVector.length op)-1 do 
                       match CCVector.get op i with
                       | NewOptionError -> (
                           Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: unexpected command: \'%s\'\027\n" Sys.argv.(i+2);
                           exit 1)
                       | NewOptionHelp -> Printf.printf "help"
                   done;))
           | CommandKindRun op -> (
               match Array.length Sys.argv with
               | 2 -> Printf.printf "help"
               | _ -> (
                   for i = 0 to (CCVector.length op)-1 do 
                       match CCVector.get op i with
                       | RunOptionFile f -> LilyCommand.lily_run f ()
                       | RunOptionFileError -> (
                           Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: file doesn\'t specified\027\n";
                           exit 1)
                       | RunOptionHelp -> Printf.printf "help"
                   done;))
           | CommandKindTo op -> (
               match Array.length Sys.argv with
               | 2 -> Printf.printf "help"
               | _ -> (
                   for i = 0 to (CCVector.length op)-1 do
                       match CCVector.get op i with
                       | ToOptionError -> (
                           Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: unexpected command: \'%s\'\027\n" Sys.argv.(i+2);
                           exit 1)
                       | ToOptionHelp -> Printf.printf "help"
                   done;))
           | CommandKindVersion -> LilyCommand.lily_version ()
