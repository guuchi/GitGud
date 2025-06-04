let sha1 s =
  let cmd = "shasum" in
  let (ic, oc) = Unix.open_process cmd in
  output_string oc s;
  close_out oc;
  let line = input_line ic in
  Scanf.sscanf line "%s " (fun x -> x)

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let write_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let zlib_compress data =
  let temp_in = Filename.temp_file "wyag_in" ".tmp" in
  let temp_out = Filename.temp_file "wyag_out" ".gz" in
  write_file temp_in data;
  let _ = Sys.command (Printf.sprintf "gzip -c %s > %s" temp_in temp_out) in
  let result = read_file temp_out in
  Sys.remove temp_in;
  Sys.remove temp_out;
  result

let zlib_decompress data =
  let temp_in = Filename.temp_file "wyag_in" ".gz" in
  let temp_out = Filename.temp_file "wyag_out" ".tmp" in
  write_file temp_in data;
  let _ = Sys.command (Printf.sprintf "gzip -cd %s > %s" temp_in temp_out) in
  let result = read_file temp_out in
  Sys.remove temp_in;
  Sys.remove temp_out;
  result

let hex_to_bytes hex =
  let len = String.length hex / 2 in
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    let byte = int_of_string ("0x" ^ String.sub hex (i*2) 2) in
    Bytes.set bytes i (Char.chr byte)
  done;
  Bytes.to_string bytes

let build_entry mode filename sha =
  let b = Buffer.create 256 in
  Buffer.add_string b mode;
  Buffer.add_char b ' ';
  Buffer.add_string b filename;
  Buffer.add_char b '\000';
  Buffer.add_string b (hex_to_bytes sha);
  Buffer.contents b