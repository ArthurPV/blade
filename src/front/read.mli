type 'a read = {
  filename: string;
  content: string;
  length: int;
  mutable c: char;
}

val new_read : string -> string -> ('a read, string) result

module GetFileContent : sig
    val msg_not_exists : string -> unit

    val msg_is_dir : string -> unit

    val msg_bad_extensions : string -> unit

    val read_lines : string -> string list

    val get_file_content : string list -> (string, string) result
end
