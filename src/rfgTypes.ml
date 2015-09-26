
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

let rec count_files = function
  | Folder(f) -> f.count
  | RegularFile(f) -> 1
and space_used = function
  | Folder(f) -> List.fold_left (fun sum file -> sum + (space_used file)) 0 f.files
  | RegularFile(f) -> f.size
;;
