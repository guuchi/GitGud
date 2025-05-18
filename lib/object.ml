open Repository
open Util

type git_object =
  | Blob of string
  | Commit of (string * string)
  | Tree of (string * string list)

let object_read repo sha =
  let path = Filename.concat repo.gitdir "objects" |> Filename.concat (String.sub sha 0 2) in
  let file = Filename.concat path (String.sub sha 2 ((String.length sha) - 2)) in
  let compressed = read_file file in
  let data = zlib_decompress compressed in
  match String.split_on_char '\000' data with
  | header :: body :: _ ->
      begin match String.split_on_char ' ' header with
      | "blob" :: _ -> Blob body
      | "commit" :: _ ->
          let lines = String.split_on_char '\n' body in
          let tree_line = List.find (fun l -> String.starts_with ~prefix:"tree " l) lines in
          let msg = List.nth lines (List.length lines - 2) in
          Commit (String.sub tree_line 5 (String.length tree_line - 5), msg)
      | "tree" :: _ ->
          let entries = String.split_on_char '\n' body |> List.filter (fun s -> s <> "") in
          Tree ("", entries)
      | _ -> failwith "Unknown object type"
      end
  | _ -> failwith "Invalid object format"

let object_write repo obj =
  Printf.printf "Appel object_write\n%!";
  let (header, content) = match obj with
    | Blob s -> ("blob", s)
    | Commit (tree_sha, msg) -> ("commit", "tree " ^ tree_sha ^ "\n\n" ^ msg ^ "\n")
    | Tree (_, entries) -> ("tree", String.concat "\n" entries)
  in
  let store = header ^ " " ^ string_of_int (String.length content) ^ "\000" ^ content in
  let sha = sha1 store in

  Printf.printf "Écriture de l’objet %s (%d octets)\n%!" sha (String.length store);
  print_newline ();

  let objects_dir = Filename.concat repo.gitdir "objects" in
  let path = Filename.concat objects_dir (String.sub sha 0 2) in
  let file = Filename.concat path (String.sub sha 2 (String.length sha - 2)) in
  if not (Sys.file_exists path) then mkdir_p path;
  write_file file (zlib_compress store);
  sha