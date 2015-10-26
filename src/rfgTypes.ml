
type regular_file_t = {
  filepath: string; (** File path *)
  checksum: string; (** md5 sum currently *)
  size: int; (** file size *)
}

and folder_t = {
  folderpath: string; (** Folder path *)
  files: file_t list; (** Sub files *)
  count: int; (** amount of files *)
}

and file_t =
  RegularFile of regular_file_t
  | Folder of folder_t
;;


(* Accessors *)
(** count number of files inside tree (folders are not counted) *)
let rec count_files = function
  | Folder(f) -> f.count
  | RegularFile(f) -> 1
(** compute space used
 * It is not real, because it count only regulare file sizes *)
and space_used = function
  | Folder(f) -> List.fold_left (fun sum file -> sum + (space_used file)) 0 f.files
  | RegularFile(f) -> f.size
(** return the path of the file *)
and path = function
  | Folder(f) -> f.folderpath
  | RegularFile(f) -> f.filepath
(** iterate through files, and apply fct to each file *)
and iter fct = function
  | Folder(f) -> List.iter (fun e -> iter fct e) f.files
  | RegularFile(f) -> fct f
(** Fold left on files *)
and fold_left fct initial = function
  | Folder(f) -> List.fold_left (fun a e -> fold_left fct a e) initial f.files
  | RegularFile(f) -> fct f
;;
