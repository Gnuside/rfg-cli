open Random
open Arg
open RfgTypes
open FolderGenerator
open Tools

let version = "0.9.0";;

let seed = ref None
and file_max_size = ref 500
and file_min_size = ref 3
and amount_of_data = ref (1024)
and number_of_files = ref 1000
and folder = ref ""
and command = ref ""
;;

let args = [
  ("--nb-files", Set_int(number_of_files), "Set total amount of files created (folders are not included).");
  ("--min-file-size", Set_int(file_min_size), "Set the minimum file size in KB (by default 3 KB).");
  ("--max-file-size", Set_int(file_max_size), "Set the maximum file size in KB (by default 500 KB).");
  ("--amount-data", Set_int(amount_of_data), "Set the maximum space used by the generator in MB (by default 1 GB).");
  ("--folder", Set_string(folder), "Set folder path to analyse with check command");
  ("--seed", Int(fun s -> seed := Some(s)), "Set initial seed for Random function (by default use /dev/urandom if available).");
];;

let anon_fun arg =
  if !command != "" then command := arg
  else failwith (Printf.sprintf "No than one argument please : '%s' ignored." arg)
;;

let usage_msg =
  (Printf.sprintf "%s [create] or [check --folder folder-path]" Sys.executable_name) ^ "\n\r"
  ^ (Printf.sprintf "\tVersion: %s" version) ^ "\n\r"
  ^ "\tcreate command allows you to create a folder containing randomly made data (files and folder tree)." ^ "\n\r"
  ^ "\tcheck command allows you to check if the specified folder data hasn't changed." ^ "\n\r"
;;

Arg.parse args anon_fun usage_msg;;

let check () =
  if !folder = "" then
    failwith "Please specify folder path.";
  let folder_clean = remove_trailing_slash !folder in
  let folder_desc = folder_clean ^ ".bin.desc" in
  let folder_desc_checksum = folder_clean ^ ".checksum" in
  let folder_desc_checksum_ic = open_in folder_desc_checksum in
  let folder_desc_checksum_line = input_line folder_desc_checksum_ic
  and folder_desc_actual_checksum = FolderGenerator.checksum_resume_string folder_desc in
  if folder_desc_checksum_line <> folder_desc_actual_checksum then begin (* FIXME: does not work if the file path is different (with ./ at the beginning for example) *)
    print_endline folder_desc_checksum_line;
    print_endline folder_desc_actual_checksum;
    failwith (folder_desc ^ " and " ^ folder_desc_checksum ^ " does not match.")
  end;
  let folder_desc_ic = open_in_bin folder_desc in
  let folder = (Marshal.from_channel folder_desc_ic : RfgTypes.file_t)
  and check_and_show_errors = function
    | Ok         -> print_endline "OK"
    | Errors(es) -> let show_error e = print_endline ("KO " ^ e.file.filepath)
                    in List.iter show_error es
  in check_and_show_errors (FolderGenerator.check folder)
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
  FolderGenerator.serialize_folder_description folder_description folder;
  FolderGenerator.create_checksum_resume checksum_resume folder_description
;;

let _ =
  match !command with
  | "check" -> check ()
  | "" | "create" -> create ()
  | cmd -> failwith (Printf.sprintf "%s: unknown command" cmd)
;;
