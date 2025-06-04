open Bytes
open Filename

exception InvalidArgument of string * string

(* LES TYPES : gitRepo qui est un repo git (!!) et command qui liste les commandes (!!)*)

type gitRepo = {
  mutable worktree : string;
  mutable gitdir : string;
  mutable conf : string;
}


type gitTreeLeaf = {
  mutable mode : bytes;
  mutable path : string;
  mutable sha : string;
}


type gitCommit = {
  mutable temp : string;
}


type command =
  | Push
  | Backup of string
  | Init of string 
  | Add of string list
  | Cat of string * string
  | Hash of string * string * bool (* type, fichier, true si on stocke dans le repo, false sinon *) 
  | Commit of string
  | Log 
  | Checkout of string
  | Branch_list
  | Branch_create of string 
  | Branch_checkout of string
  | Merge of string
  | Remove of string
  | Link of string

(* Diverses fonctions utilitaires *)

let enleve_dernier l = List.rev @@ List.tl @@ List.rev l;;
let separe_en_deux lst = 
  if (List.length lst < 2) then (lst, [])
  else begin
    let premier = List.hd lst in 
    let lst2 = List.tl lst in 
    let deuxieme = List.hd lst2 in 
    let retour = List.tl lst2 in 
    (premier::[deuxieme], retour)
  end;;

let rec strictAvant lst = function
  | 0 -> []
  | n -> (List.hd lst)::(strictAvant (List.tl lst) (n - 1));;

let rec apres lst = function
  | 0 -> lst 
  | n -> apres (List.tl lst) (n - 1);;

let slicing lst a b = 
  apres (strictAvant lst b) a;;

let rec print_lst = function 
  | [] -> print_string "\n" 
  | t::q -> print_int t; print_lst q;;

let aux_find str substr = 
  (* on implemente comme un golmon car les substr sont petits*)
  let n = String.length str and m = String.length substr in 
  let id = ref 0 in 
  let ret = ref (-1) in 
  while ((!id + m) <= n) do 
    let ok = ref true in 
    for d = 0 to (m - 1) do 
      ok := (!ok)&&(substr.[d] = str.[!id + d])
    done;

    if (!ok && !ret = -1) then ret := !id;
    incr id;
  done;
  !ret;;

let rec repo_find path =
  (*On supprime le required et on retourne toujours "" si il n'y a pas de repo*) 
  if (Sys.file_exists (Sys.concat path ".git")) && (Sys.is_directory (Sys.concat path ".git")) then
    aux_init path
  else
    let av = Filename.realpath (Sys.concat path "..") in
    if av == path then
      ""
    else
      repo_find av

let utils_find str substr deb = 
  let n = String.length str in 
  aux_find (String.sub str deb (n - deb)) substr;; (* j'ai change le nom*)

let rec aux_replace str ancien nouveau deb = match (outils_find str ancien deb ) with 
  (* implemente comme un golmon en n^3*)
  | -1 -> str 
  | i -> 
    let len_ancien = String.length ancien in 
    let len_str = String.length str in 
    let premier_changement = (String.sub str 0 i)^nouveau^(String.sub str (i + len_ancien) (len_str - i - len_ancien)) in 

    aux_replace premier_changement ancien nouveau (i + len_ancien);;

let replace str ancien nouveau = 
  aux_replace str ancien nouveau 0;;(**)