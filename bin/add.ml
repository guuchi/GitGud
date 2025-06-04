let add regexp =
  print_endline regexp ;
  0



open Cmdliner
open Cmdliner.Term.Syntax

let exp = 
  let doc = "Expression régulière reconnaissant les fichiers à ajouter" in
  Arg.(value & pos 0 string "*" & info [] ~doc ~docv:"EXPR")

let cmd =
  let doc = "Met à jour l'index en utiliseant le contenu trouvé dans l'arbre de travail, pour conserver le contenu lors du prochain commit"
  in
  Cmd.v (Cmd.info "add" ~doc) @@
  let+ exp in
  add exp


  