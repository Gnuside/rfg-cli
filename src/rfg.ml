open Random
open Arg
open RfgTypes
open FolderGenerator

let version = "0.9.0";;

let seed = ref None
and file_max_size = ref 500
and file_min_size = ref 3
and amount_of_data = ref (1024)
and number_of_files = ref 1000
and folder_desc = ref ""
and command = ref ""
;;

let args = [
  ("--nbfiles", Set_int(number_of_files), "Set total amount of files created (folders are not included).");
  ("--min-file-size", Set_int(file_min_size), "Set the minimum file size in KB (by default 3 KB).");
  ("--max-file-size", Set_int(file_max_size), "Set the maximum file size in KB (by default 500 KB).");
  ("--amount-data", Set_int(amount_of_data), "Set the maximum space used by the generator in MB (by default 1 GB).");
  ("--folder-desc", Set_string(folder_desc), "Set folder desc file to analyse with check command");
  ("--seed", Int(fun s -> seed := Some(s)), "Set initial seed for Random function (by default use /dev/urandom if available).");
];;

let anon_fun arg =
  if !command != "" then command := arg
  else failwith (Printf.sprintf "No than one argument please : '%s' ignored." arg)
;;

let usage_msg =
  (Printf.sprintf "%s [create] or [check --folder-path folder-path]" Sys.executable_name) ^ "\n\r"
  ^ (Printf.sprintf "\tVersion: %s" version) ^ "\n\r"
  ^ "\tcreate command allows you to create a folder containing randomly made data (files and folder tree)." ^ "\n\r"
  ^ "\tcheck command allows you to check if the specified folder data hasn't changed." ^ "\n\r"
;;

Arg.parse args anon_fun usage_msg;;

let check () =
  print_endline "TODO";
  (if !folder_desc = "" then
    failwith "Please specify folder-desc file.");
  let folder_desc_ic = open_in_bin !folder_desc in
  let folder = (Marshal.from_channel folder_desc_ic : RfgTypes.file_t) in
  print_file_t folder
;;

let create () =
  let max_amount = !amount_of_data * 1024 in
  (match !seed with
  None -> Random.self_init ()
  | Some(s) -> Random.init s
  );
  Tools.init_status !number_of_files max_amount !file_max_size;
  let folder = FolderGenerator.create ~min_size:!file_min_size ~max_files:!number_of_files ~max_amount ~max_size:!file_max_size () in
  let folder_description = (RfgTypes.path folder) ^ ".bin.desc"
  and checksum_resume = (RfgTypes.path folder) ^ ".checksum" in
  let fd_o = open_out_bin folder_description
  and cr_o = open_out checksum_resume in
  Marshal.to_channel fd_o folder [Marshal.No_sharing];
  close_out fd_o;
  output_string cr_o (Printf.sprintf "%s: %s"
    folder_description
    (Tools.hex_string (Digest.file folder_description))
  );
  close_out cr_o
;;

let _ =
  match !command with
  | "check" -> check ()
  | "" | "create" -> create ()
  | cmd -> failwith (Printf.sprintf "%s: unknown command" cmd)
;;

