type read = {
  filename: string;
  content: string;
  length: int;
  mutable c: char;
}

let new_read filename content =
    if String.length content >= 1 then
        Ok {
            filename = filename;
            content = content;
            length = String.length content;
            c = content.[0]
        }
    else
       Error "empty file"

module GetFileContent = struct
    let msg_not_exists filename =
        Printf.printf "\027[1m\027[31mError\027[0m\027[1m: the file doesn\'t exists: \'%s\'\027[0m\n" filename;
        exit 1

    let msg_is_dir filename =
        Printf.printf "\027[1m\027[31mError\027[0m\027[1m: the file is a directory: \'%s\'\027[0m\n" filename;
        exit 1

    let msg_bad_extensions filename = 
        Printf.printf "\027[1m\027[31mError\027[0m\027[1m: bad extensions of file: \'%s\'\027[0m\n" filename;
        exit 1

    let read_lines filename =
        if Sys.file_exists filename <> true then
            msg_not_exists filename
        else
            if Sys.is_directory filename <> false then
                msg_is_dir filename
            else 
                if Filename.extension filename <> ".li" then
                   msg_bad_extensions filename
                else
                    let ic = open_in filename in
                    let try_read () =
                        try Some (input_line ic) with End_of_file -> None in
                    let rec loop acc =
                        match try_read () with
                        | Some s -> loop (s :: acc)
                        | None -> close_in ic; List.rev acc in
                    loop []

    let get_file_content content =
        match content with
        | [] -> Error ""
        | _ -> Ok (let add_newline x = x ^ "\n" in
                   String.concat "" (List.map add_newline content))
end
