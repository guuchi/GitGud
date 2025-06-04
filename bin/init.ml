open Outils
open Unix
open Sys
open Filename
(*open Inifiles
*)
exception InvalidArgument of string * string;;

let rec repo_path repo pathlist = List.fold_left Filename.concat repo.gitdir pathlist;;


let repo_dir repo pathlist mkdir = 
  let new_path = repo_path repo pathlist in 

  if Sys.file_exists new_path then begin
    if Sys.is_directory new_path then begin
      new_path end
    else begin
      raise (InvalidArgument ("Pas un répertoire !", new_path))
    end
  end
  else begin
    if mkdir then begin
      Sys.mkdir new_path 0o755; new_path
    end
    else ""
  end;;

let repo_file repo path mkdir = 
  if (repo_dir repo (enleve_dernier path) mkdir != "") then repo_path repo path else "";; 

  (*
let aux_init path = 
  let gitdir = Sys.concat path ".git" in

  if not (Sys.is_directory gitdir) then begin
    raise Outils.InvalidArgument("Pas un repertoire Git !", path) end

  let cf = Sys.concat gitdir "config" in
  if not (Sys.file_exists cf) then
    raise Outils.InvalidArgument("Il manque les fichiers de configuration", path);
  conf = Inifiles.inifile cf in 
  let repo = {worktree=path; gitdir=gitdir; conf=conf}

  let vers = int_of_string (repo.conf.getval "core" "repositoryformatversion") in 
  if (vers <> 0) then raise Outils.InvalidArgument("Unsupported repositoryformatversion: ", string_of_int vers);
  repo

(*CHOSES A COMPLETER ICI*)

let create_repo_default_config path =
  let defaultconf = "[core]\nrepositoryformatversion = 0\n filemode = false\nbare = false" in
  let sortie_confi = open_out path in
  Printf.fprintf sortie_confi defaultconf;
  close_out sortie_confi;

let repo_create path =
  
  let gitdir = Sys.concat path ".git" in
  if (Sys.file_exists path) then begin 
    if not (Sys.is_directory path) then 
      raise Outils.InvalidArgument("Ce n'est pas un répertoire !", path);
    if (Sys.listdir path) != [] then 
      raise Outils.InvalidArgument("Ce répertoire n'est pas vide !", path);
  end 
  else
    Sys.mkdir path 0o755;
  Sys.mkdir gitdir 0o755;
  create_repo_default_config (Sys.concat gtidir "config");
  let repo = aux_init path in

  repo_dir repo ["branches"] true;
  repo_dir repo ["objects"] true;
  repo_dir repo ["refs"; "tags"] true;
  repo_dir repo ["refs"; "heads"] true;

  
  let sortie_description = open_out (repo_file repo "description") in 
  Printf.fprintf sortie_description "Unnamed repository; edit this file 'description' to name the repository.\n";
  close_out sortie_description;

  let sortie_head = open_out (repo_file repo "HEAD") in 
  Printf.fprintf sortie_head "ref: refs/heads/master\n";
  close_out sortie_head;
  repo
  
  


let init path =

  repo_create path;




open Cmdliner
open Cmdliner.Term.Syntax

let path = 
  let doc = "Chemin où initialiser le git" in
  Arg.(value & pos 0 string "." & info [] ~doc ~docv:"PATH")

let cmd =
  let doc = "Crée un nouveau répertoire à l'emplacement désigné."
  in
  Cmd.v (Cmd.info "init" ~doc) @@
  let+ path in
  init path*)