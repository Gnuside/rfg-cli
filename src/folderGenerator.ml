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
 * - @max_amount: how much data space will be took by files in any case
 * This returns an RfgTypes.file_t :
   * - folderpath
   * - List of Files/folders
   * - Actual number of sub-files
 * Warning: All the limitations are approximatives, not strict
 *)
let rec create ?(depth=0) ?min_files:(mif=2) ?max_files:(maf=50) ?(min_size=3) ?(max_size=5242880) ?(max_amount=5242880) ?foldername:(fn=new_name ()) ?path:(p=".") () =
  (*print_endline (Printf.sprintf
    "FolderGenerator.create min_files:%d max_file:%d min_size:%d max_size:%d foldername:%s path:%s"
    mif maf min_size max_size fn p);*)
  let folder_path = (p ^ "/" ^ fn)
  and current_files = ref 0
  and stop = ref false
  and res = ref [] (* files created *)
  and amount_data = ref 0
  in
  Tools.refresh_status ~depth folder_path max_amount 0 ~cnf:0 ~cmf:maf "folder";
  Unix.mkdir folder_path 0o750;
  while (not !stop) && (!current_files < maf || !amount_data < max_amount) do
    (*print_endline (Printf.sprintf "Made %d files over %d" !current_files maf);*)
    let file_to_create =
      if !current_files > mif && depth != 0 then
        get_random 0 21 (* If we are not > min_files we don't allow 20 value which prevent us to stop *)
      else if (!current_files < maf && !amount_data < max_amount) then
        get_random 0 20
      else
        get_random 0 14 (* only select files *)
    in
    (*print_endline (Printf.sprintf "file_to_create: %d" file_to_create);*)
    if file_to_create < 13 then begin
      (* We create a file *)
      (*print_endline "Creating a file";*)
      let file = FileGenerator.create ~min_size ~max_size:(get_random 0 (min max_size (max_amount - !amount_data))) ~path:(folder_path) () in
      let size = space_used file in
      res := !res @ [ file ];
      amount_data := !amount_data + size;
      incr current_files
    end else if file_to_create < 20 then begin
      (* We create a folder *)
      (*print_endline "Creating a folder";*)
      let folder = create ~depth:(depth+1) ~min_size ~max_size ~min_files:(mif - !current_files)
        ~max_files:(get_random 0 (maf - !current_files))
        ~max_amount:(get_random 0 (max_amount - !amount_data))
        ~path:(folder_path) ()
      in
      let size = space_used folder
      and nb_files = count_files folder in
      res := !res @ [ folder ];
      amount_data := !amount_data + size;
      current_files := !current_files + nb_files (* we don't count folder themself *)
    end else begin
      (* We stop looping *)
      (* print_endline "Stoping"; *)
      stop := true
    end;
    Tools.refresh_status ~depth folder_path max_amount !amount_data ~cnf:!current_files ~cmf:maf "folder";
  done;
  Tools.refresh_status ~depth folder_path max_amount !amount_data ~cnf:!current_files ~cmf:maf "folder";
  let folder = Folder {folderpath = folder_path; files = !res; count = !current_files } in
  (*Printf.printf "\nFolder (%d) created: %d/%d KB inside with %d/%d files\n" depth (space_used folder) max_amount !current_files maf;*)
  folder
;;

let check folder =
  iter FileGenerator.check folder
;;

let rec print_file_t ?level:(level=0) = function
  | Folder(f) -> print_file_t_folder level f
  | RegularFile(f) -> print_file_t_file level f
and print_file_t_folder level (f:folder_t) =
  let space_level = String.make (level*2) ' ' in
  let print_indent () = print_string space_level in
  print_indent (); print_endline "Folder";
  print_indent (); print_endline ("path: " ^ f.folderpath);
  print_indent (); print_endline (Printf.sprintf "count: %d" f.count);
  print_indent (); print_endline "files : [";
  List.iter (fun e -> print_file_t ~level:(level+1) e) f.files;
  print_indent (); print_endline "]"
;;
