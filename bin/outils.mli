type gitRepo = {
  mutable worktree : string;
  mutable gitdir : string;
  mutable conf : string;
}
type gitCommit = { mutable temp : string; }
type command =
    Push
  | Backup of string
  | Init of string
  | Add of string list
  | Cat of string * string
  | Hash of string * string * bool
  | Commit of string
  | Log
  | Checkout of string
  | Branch_list
  | Branch_create of string
  | Branch_checkout of string
  | Merge of string
  | Remove of string
  | Link of string
val enleve_dernier : 'a list -> 'a list
val separe_en_deux : 'a list -> 'a list * 'a list
val strictAvant : 'a list -> int -> 'a list
val apres : 'a list -> int -> 'a list
val slicing : 'a list -> int -> int -> 'a list
val print_lst : int list -> unit
val aux_find : string -> string -> int
val outils_find : string -> string -> int -> int
val aux_replace : string -> string -> string -> int -> string
val replace : string -> string -> string -> string