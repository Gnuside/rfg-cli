(** This module is in charge of creating folders
 *)

open RfgTypes
open Tools
open FileGenerator

(** Create a random name for a folder :
 * - @base_name is the base of the name used (ie new_name ?base_name:'folder-' will generate a name like 'file-34df56')
 * - @length is the length of the random part (8 by default)
 * - @end_name is the extension name (none by default, add '.' if you want an extension)
 * *)
let new_name ?length:(l=6) ?base_name:(b="folder-") ?end_name:(e="") () =
  b ^ (random_b64_string l) ^ e
;;

(** Create a random folder and fill its content with random files
 * - @max_size is the maximum size took by all the files in KB (no limit by default - limited by number of files/folders)
 *    - If @max_size not set
 * - @foldername is the name of the file (by default it is made by new_name ())
 * - @max_files: max amount of files/folder created (default: 50)
 * - @min_files: max amount of files/folder created (default: 2) ; negative value will be ignored
 * This returns an RfgTypes.file_t :
   * - folderpath
   * - List of Files/folders
   * - Actual number of sub-files
 * Warning: All the limitations are approximatives, not strict
 *)
let rec create ?min_files:(mif=2) ?max_files:(maf=50) ?max_size:(max_size=5242880) ?foldername:(fn=new_name ()) ?path:(p=".") () =
  let buf_size = 1024
  and folder_path = (p ^ "/" ^ fn)
  and current_files = ref 0
  and stop = ref false
  and res = ref [] (* files created *)
  in
  Unix.mkdir folder_path 0o750;
  while !stop || !current_files < maf do
    let file_to_create =
      if !current_files > mif then
        Random.int 21 (* If we are not > min_files we don't allow 20 value which prevent us to stop *)
      else Random.int 20
    in
    if file_to_create < 13 then begin
      (* We create a file *)
      res := !res @ [
        create ~max_size ~path:(folder_path) ()
      ];
      incr current_files
    end else if file_to_create < 20 then begin
      (* We create a folder *)
      res := !res @ [
        create ~min_files:(mif - !current_files) ~max_files:(maf - !current_files) ()
      ];
      incr current_files
    end else begin
      (* We stop looping *)
      stop := true
    end
  done;
  Folder {path = folder_path; files = !res; count = !current_files }
;;

let rec print_file_t ?level:(level=0) = function
  | Folder(f) -> print_file_t_folder level f
  | RegularFile(f) -> print_file_t_file level f
and print_file_t_folder level (f:folder_t) =
  let space_level = String.make (level*2) ' ' in
  let print_indent () = print_string space_level in
  print_indent (); print_endline "Folder";
  print_indent (); print_endline ("path: " ^ f.path);
  print_indent (); print_endline (Printf.sprintf "count: %d" f.count);
  print_indent (); print_endline "files : [";
  List.iter (fun e -> print_file_t ~level:(level+1) e) f.files;
  print_indent (); print_endline "]"
;;

