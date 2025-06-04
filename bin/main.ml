let () =
  let argv = Sys.argv |> Array.to_list |> List.tl in
  match argv with
  | "init" :: _ -> Lib.Command.cmd_init ()
  | "cat-file" :: args -> Lib.Command.cmd_cat_file args
  | "hash-object" :: args -> Lib.Command.cmd_hash_object args
  | "write-tree" :: _ -> Lib.Command.cmd_write_tree ()
  | "commit" :: args -> Lib.Command.cmd_commit args
  | "log" :: _ -> Lib.Command.cmd_log ()
  | "checkout" :: args -> Lib.Command.cmd_checkout args
  | _ ->
    Printf.printf "Usage: wyag <command>\n";
    Printf.printf "Commands: init, cat-file, hash-object, write-tree, commit, log, checkout\n"