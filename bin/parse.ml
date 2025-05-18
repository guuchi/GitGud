(* essentiellement copie de Fol parallele*)

open Obj 
(*open Outils*)

(*Message d'aide*)

let message_aide_txt = "\n\tinit [chemin] \\
                \n\tcommit [-m message]\\
                \n\tcat-file <type d'objet> <objet>\\
                \n\thash-object [-w] [-t type d'objet] <fichier>
                \n\tcheckout <sha>
                \n\tbranch_create <name>
                \n\tbranch_list
                \n\tbranch_checkout <name>
                \n\tmerge <branch>
                \n\tlink <url>
                "

let message_aide erreur = 
  print_string ("Erreur"^erreur^"\nMessage générique :"^message_aide_txt);;


let mauvaise_quantite_arguments = (message_aide "Mauvais nombre d'arguments"; exit 1)

let parse_init (l : string list) : command =
  match l with
  | [path] -> Init path
  | [] -> Init "."
  | _ -> mauvaise_quantite_arguments

(*Implémenter des regexp ici*)
let parse_add (l : string list) : command = Add l


let parse_commit (l : string list) : command =
  match l with
  | "-m" :: [msg] -> Commit msg
  | [] -> Commit ""
  | _ -> mauvaise_quantite_arguments

let parse_catfile (l : string list) : command =
  match l with
  | [otype; obj] -> Cat (otype, obj)
  | _ -> mauvaise_quantite_arguments

let parse_hash (l : string list) : command =
  let getstore (l : string list) : bool * (string list) =
    (List.mem "-w" l, List.filter ((=) "-w") l) in     
  let get_type (l : string list) : string * (string list) =
    let indt = List.find_index (fun x -> x = "-t") l in
    (match indt with
     | None -> ("blob", l)
     | Some i ->         
        if (List.length l) - 1 <> i then
          (List.nth l (i+1), List.filteri (fun j _ -> j <> i && j <> (i+1)) l)  
        else
          (message_aide "-t : type d'objet non spécifié"; exit 1)) in
  let get_fname (l : string list) : string =
    if List.length l <> 1 then
      mauvaise_quantite_arguments
    else
      List.hd l
  in
  let s, l = getstore l in
  let t, l = get_type l in
  let n = get_fname l in
  Hash (t, n, s)   
          
let parse_checkout args = Checkout (List.hd args)

let parse_branch_create args = Branch_create (List.hd args)

let parse_branch_checkout args = Branch_checkout (List.hd args)
 
let parse_merge args =  Merge (List.hd args)

let parse_link args = Link (List.hd args) 

let parse_backup args = Backup (List.hd args)

let parse_push = Push 


let parse_global () : command =
  let arguments = List.tl @@ Array.to_list @@ Sys.argv in
  match arguments with
  | h  :: tl ->
     (match h with
      | "add" -> parse_add tl
      | "branch_create" -> parse_branch_create tl
      | "branch_checkout" -> parse_branch_checkout tl 
      | "branch_list" -> Branch_list
      | "cat-file" -> parse_catfile tl
      | "commit" -> parse_commit tl 
      | "checkout" -> parse_checkout tl
      | "log" -> Log
      | "init" -> parse_init tl 
      | "hash-object" -> parse_hash tl
      | "merge" -> parse_merge tl
      | "remove" -> Remove(List.hd tl)
      | "link" -> parse_link tl
      | "backup" -> parse_backup tl
      | "push" -> parse_push
      | _ -> message_aide ("Commande "^h^" inconnue"); exit 1)
  | _ -> message_aide "Aucune commande n'a été entrée"; exit 1