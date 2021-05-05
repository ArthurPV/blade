open Blade_front.Read

let run filename =
    match GetFileContent.get_file_content (GetFileContent.read_lines filename) with
    | Ok s -> Printf.printf "%s" s
    | Error e -> Printf.printf "%s" e
