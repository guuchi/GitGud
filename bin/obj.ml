let buffer_size = 4096;;

let decompress_file source =
  let gz_file = open_in source in
  let buffer = Bytes.make buffer_size '*' in

  let rec aux deb =
    let len = input gz_file buffer deb buffer_size in
    if len <> 0 then
        aux (deb + len);
  in aux 0;
  buffer;;

let object_read repo sha =
  let (first_arg, second_arg) = Outils.separe_en_deux sha in 
  let path = Init.repo_file repo "objects" first_arg second_arg in 

  if (not Sys.is_regular_file path) then begin
    None 
  end;

  try while true do 
    let f = open_in path in 
    let raw = decompress_file path in
    let x = aux_find raw " " in 
    let fmt = strictAvant raw x  in 

    let y = find raw "\x00" x in 
    let size = (slicing raw x y).decode("ascii") in 

   (* if (size <> (List.length raw - y - 1)) then
      (*raise "Objet malformé {sha} : mauvaise longueur" *) ()
*)
    let dans_cons = apres raw y + 1 in 
    match fmt with 
      | "commit" -> GitCommit(dans_cons) (* Todo : modifier avec les bons constructeurs*)
      | "tree" -> GitTree(dans_cons)
      | "tag" -> GitTag(dans_cons)
      | "blob" -> GitBob(dans_cons)
      | _ -> (*raise "Type {fmt.decode("ascii")} inconnu pour l'objet {sha}"*) GitBob(dans_cons)
  done; (* j'ai un doute sur la syntaxe caml, mais c cense renvoyer ca*)
  with _ -> ();;



let object_find repo name fmt follow = name (* a modifier *)

let object_write obj repo = 
  let data = serialize obj in (* todo : implementer serialize *)
  let result = obj.fmt^" "^str(len(data)).encode()^"\x00"^data in 
  let sha = sha1 result in

  if (repo <> 42) then begin 
    (*juste if (repo) dans le code python, a remplacer par une valeur sérieuse*)
    let (first_arg, second_arg) = separe_en_deux sha in 
    let path = repo_file repo "objects" first_arg second_arg true in 

    if (not (Sys.path_exists path)) then begin 
      let f = open_out path in 
      output f (Bytes.of_string result)
    end
  end
  sha ;;

let cat_file repo obj fmt = (* fmt est optionnel en python*)
  let obj = object_read repo (object_find repo obj fmt) in 
  Printf.printf (serialize obj) (* dans le code python c sys.stdout.buffer.write obj.serialize()*)

let cmd_cat_file args = 
  let repo = repo_find "." in 
  cat_file repo args.objects 

let object_hash fd fmt repo = 
  let data = read fd in (*fd.read() en python*)

  let obj = match fmt with 
    | "commit" -> Commit(data)
    | "tree" -> GitTree(data)
    | "tag" -> GitTag(data)
    | "blob" -> GitBlob(data)
    | _ -> raise "Type {fmt} inconnu ! "
  in object_write obj repo 

(*ATTENTION : IL FAUT AJOUTER CMD_HASH_OBJECT*)