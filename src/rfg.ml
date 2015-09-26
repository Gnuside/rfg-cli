open Random
open Arg
open RfgTypes
open FolderGenerator

let version = "0.1.0";;

let seed = ref None
and file_max_size = ref 500
and amount_of_data = ref (1024)
and number_of_files = ref 1000
;;

let args = [
  ("--nbfiles", Set_int(number_of_files), "Set total amount of files created (folders are not included).");
  ("--max-file-size", Set_int(file_max_size), "Set the maximum file size in KB (by default 500 KB).");
  ("--amount-data", Set_int(amount_of_data), "Set the maximum space used by the generator in MB (by default 1 GB).");
  ("--seed", Int(fun s -> seed := Some(s)), "Set initial seed for Random function (by default use /dev/urandom if available).");
];;

let anon_fun arg =
  failwith (Printf.sprintf "No anonyme argument please : '%s'." arg)
;;

let usage_msg =
  (Printf.sprintf "%s: see options below." Sys.executable_name) ^ "\n\r"
  ^ (Printf.sprintf "\tVersion: %s" version) ^ "\n\r"
;;

Arg.parse args anon_fun usage_msg;;

let _ =
  (match !seed with
  None -> Random.self_init ()
  | Some(s) -> Random.init s
  );
  print_file_t (FolderGenerator.create ~max_files:!number_of_files ~max_amount:(!amount_of_data * 1024) ~max_size:!file_max_size ())
;;
