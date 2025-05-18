type repo = {
  worktree : string;
  gitdir : string;
}

let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if parent <> path then mkdir_p parent;
    Unix.mkdir path 0o755
  end

let repo_create path =
  Printf.printf "test"; print_newline (); 
  let gitdir = Filename.concat path ".git" in
  let dirs = [
    "branches";
    "objects";
    "refs/tags";
    "refs/heads";
  ] in
  List.iter (fun d ->
    let full = Filename.concat gitdir d in
    mkdir_p full  (* Utilise mkdir_p au lieu de Unix.mkdir *)
  ) dirs;

  let write path content =
    let full = Filename.concat gitdir path in
    let oc = open_out full in
    output_string oc content;
    close_out oc
  in
  write "HEAD" "ref: refs/heads/master\n";
  write "config" "[core]\n\trepositoryformatversion = 0\n\tfilemode = false\n\tbare = false\n";
  write "description" "Unnamed repository.\n";
  { worktree = path; gitdir }