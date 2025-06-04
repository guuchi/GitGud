
open Repository
open Object
open Util
open Sys

let cmd_init () =
  let cwd = getcwd () in
  let _ = repo_create cwd in
  Printf.printf "Initialized empty Git repository in %s/.git/\n" cwd

let cmd_cat_file args = 
  match args with
  | [sha] ->
      let repo = { worktree = "."; gitdir = "./.git" } in
      Printf.printf "[DEBUG] Lecture de l’objet %s\n" sha;
      begin match object_read repo sha with
      | Blob content -> Printf.printf "[BLOB]\n%s\n" content
      | Commit (tree, msg) -> Printf.printf "[COMMIT]\ntree: %s\nmessage: %s\n" tree msg
      | Tree (_, entries) -> 
          Printf.printf "[TREE]\n";
          List.iter print_endline entries
      end
  | _ -> prerr_endline "Usage: wyag cat-file <object-hash>"
  
let cmd_hash_object args =
  match args with
  | [path] ->
      let content = read_file path in
      let repo = { worktree = "."; gitdir = "./.git" } in
      let sha = object_write repo (Blob content) in
      Printf.printf "%s\n" sha
  | _ -> prerr_endline "Usage: wyag hash-object <file>"

let is_file f =
  try Sys.is_directory f |> not with _ -> false

let cmd_write_tree () =
  Printf.printf "[DEBUG] cmd_write_tree appelé\n%!";
  let repo = { worktree = "."; gitdir = "./.git" } in
  
  let files = Sys.readdir "." |> Array.to_list|> List.filter (fun f -> f <> ".git" && is_file f) in

  Printf.printf "[DEBUG] fichiers : [%s]\n%!" (String.concat "; " files);
 (* Printf.printf "ff 2\n"; print_newline (); *)
  let entries = List.map (fun f ->
    let content = read_file f in
    let sha = object_write repo (Blob content) in
    build_entry "100644" f sha
  ) files in
  let sha = object_write repo (Tree ("", entries)) in
  Printf.printf "%s\n" sha

let cmd_commit args =
  match args with
  | [tree_sha; msg] ->
      let repo = { worktree = "."; gitdir = "./.git" } in
      let sha = object_write repo (Commit (tree_sha, msg)) in
      write_file (Filename.concat repo.gitdir "HEAD") sha;
      Printf.printf "%s\n" sha
  | _ -> prerr_endline "Usage: wyag commit <tree-sha> <message>"

let cmd_log () =
  let repo = { worktree = "."; gitdir = "./.git" } in
  let log sha =
    match object_read repo sha with
    | Commit (tree, msg) ->
        Printf.printf "commit %s\n" sha;
        Printf.printf "tree %s\n" tree;
        Printf.printf "    %s\n\n" msg
    | _ -> ()
  in
  let sha = read_file (Filename.concat repo.gitdir "HEAD") in
  log (String.trim sha)

let cmd_checkout args =
  match args with
  | [sha] ->
      let repo = { worktree = "."; gitdir = "./.git" } in
      begin match object_read repo sha with
      | Tree (_, entries) ->
          List.iter (fun e ->
            try
              let parts = String.split_on_char '\000' e in
              let info = List.hd parts in
              let sha = List.nth parts 1 in
              let name = List.nth (String.split_on_char ' ' info) 1 in
              match object_read repo sha with
              | Blob content -> write_file name content
              | _ -> ()
            with _ -> ()
          ) entries
      | _ -> prerr_endline "Not a tree object"
      end
  | _ -> prerr_endline "Usage: wyag checkout <tree-sha>"