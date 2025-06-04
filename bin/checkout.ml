open Obj
open Outils
open Sys
open Filename
open Out_channel 

let normalize_to_six = function 
  | str when (String.length str = 5) -> "0"^str 
  | str -> str;;

let tree_parse_one raw start = 
  let x = outils_find raw " " start in 
  assert (x-start == 5 || x-start==6);

  let mode = normalize_to_six (String.sub raw start (x - start)) in 
  let y = outils_find raw "\x00" x in 
  let path = String.sub raw (x+1) (y - x - 1) in 
  let raw_sha = int_of_string (String.sub raw (y + 1) 20) in 
  let sha = raw_sha in 
  let git_tree_leaf = {mode = mode};

  (y+21, {mode = mode;path = path;sha =sha});;


let tree_parse raw = 
  let pos = ref 0 in
  let maxi = String.length raw in 
  let ret = ref [] in 

  while (!pos < maxi) do 
    let (tmp, data) = tree_parse_one raw !pos in 
    pos := tmp;
    ret := data::!ret;
  done;

  List.rev (!ret);;

let tree_leaf_sort_key leaf =
  if (String.startswith "10" leaf.mode) then leaf.path 
  else leaf.path^"/";;

let tree_checkout repo tree path =
  let feach item =
    let obj = Obj.object_read repo item.sha in
    let dest = Sys.concat path item.path in
    if obj.fmt == Bytes.of_string "tree" then begin
      Sys.mkdir dest 0o755;
      tree_checkout repo obj dest
    end
    else if obj.fmt == Bytes.of_string "blob" then
      let oc = open_out dest in
      Out.output_string obj.blobdata
  in
  List.iter feach tree.items


let checkout commit path =
  let repo = Outils.repo_find "." in
  let obj = ref Obj.object_read repo (Obj.object_find repo commit) in
  if (!obj).fmt == Bytes.of_string "commit" then
    obj := Obj.object_read repo (!obj)kvlm[b'tree'].decode("ascii");
  if Sys.file_exists path then begin
    if not (Sys.is_directory path) then
      raise Outils.InvalidArgument("Not a directory", path)
    else if Sys.listdir path then
      raise Outils.InvalidArgument("Not empty", path)
    end
  else Sys.mkdir path 0o755;
  tree_checkout repo obj (Filename.realpath path)
