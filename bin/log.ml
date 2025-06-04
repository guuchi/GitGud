open Hashtbl
open Outils
open Obj
module StringSet = Set.Make(String)

(* dct est toujours un dico de liste, alors que *)
(* dans le tuto ça peut juste etre une valeur*)

(* j'ai add les fonctions utilaires ici comme un golmon car ça marche pas la*)
let none_string = "jsp";;
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

let outils_find str substr deb = 
  let n = String.length str in 
  aux_find (String.sub str deb (n - deb)) substr;;

let rec aux_replace str ancien nouveau deb = match (outils_find str ancien deb ) with 
  (* implemente comme un golmon en n^3*)
  | -1 -> str 
  | i -> 
    let len_ancien = String.length ancien in 
    let len_str = String.length str in 
    let premier_changement = (String.sub str 0 i)^nouveau^(String.sub str (i + len_ancien) (len_str - i - len_ancien)) in 

    aux_replace premier_changement ancien nouveau (i + len_ancien);;

let replace str ancien nouveau = 
  aux_replace str ancien nouveau 0;;


let rec kvlm_parse raw start dct = 
  let spc = outils_find raw " " start in 
  let nl = outils_find raw "\n" start in 

  if (spc < 0 || nl < spc) then begin 
   (* assert (nl = start); *)
    Hashtbl.add dct none_string [String.sub raw (start + 1) (String.length raw - start - 1)];
    dct
  end
  else begin 
    let key = String.sub raw start (spc - start) in 
    let end_var = ref start in (* j'ai renomme end en end_var *)

    let continuer = ref true in 
    while (!continuer) do
      end_var := outils_find raw "\n" (!end_var + 1);
      if (raw.[!end_var + 1] <> ' ') then 
        continuer := false;
    done;

    let value = replace (String.sub raw (spc + 1) (!end_var - spc - 1)) "\n " "\n" in 
    if (Hashtbl.mem dct key) then begin 
      let lst = Hashtbl.find dct key in 
      Hashtbl.replace dct key (value::lst);
    end
    else Hashtbl.add dct key [value];
    kvlm_parse raw (!end_var + 1) dct
  end;;


let kvlm_parse_sans_dct raw start = 
  let dct = Hashtbl.create 0 in 
  kvlm_parse raw start dct;;

let kvlm_serialize kvlm = 
  let ret = ref "" in 

  Hashtbl.iter (fun key -> fun lst ->
    if (key <> none_string) then begin 
      List.iter (fun v -> 
        ret := (!ret)^key^" "^(replace v "\n" "\n ")^"\n";
      ) lst;
    end
  ) kvlm;

  ret := (!ret)^"\n"^(List.hd (Hashtbl.find kvlm none_string));
  !ret;;

let fun_test avt = 
  let raw = read_line () in 
  let dico = kvlm_parse (raw^"\n") 0 avt in 
  dico;;

let nb_tests = read_int () in 
let dict = ref (Hashtbl.create 0) in 
for i = 1 to nb_tests do 
  dict := fun_test !dict;
done;  
print_string (kvlm_serialize !dict);;


let rec log_graphviz repo sha seen = match (StringSet.mem seen sha) with 
  | true -> ()
  | false -> 
    StringSet.add seen sha;
    let commit = Obj.object_read repo sha in 
    let message_avant_avant = commit.kvlm[None].decode("utf8").strip() in 
    let message_avant = replace message_avant_avant "\\" "\\\\" in 
    let message = ref (replace message_avant "\"" "\\\"") in 

    if ((utils_find !message "\n" 0) <> -1) then 
      message := String.sub !message 0 (index_from !message 0 '\n');

    Printf.printf "  c_{sha} [label=\"{sha[0:7]}: {message}\"]"; 
    (* j'ai pas compris ce que c cense afficher*)
    assert (commit.fmt = "commit");

    if (Hashtbl.mem commit.kvlm "parent") then begin 
      let parents = Hashtbl.find commit.kvlm "parent" in 

      List.iter (fun p -> 
        Printf.printf "  c_{sha} -> c_{p};";
        log_graphviz repo p seen;
      ) parents;
    end;;

let cmd_log args = 
  let repo = repo_find in 
  Printf.printf("digraph wyaglog{\n");
  Print.printf("  node[shape=rect]\n");
  log_graphviz repo  (object_find repo args.commit) (StringSet.create );
  Printf.printf("}\n");;