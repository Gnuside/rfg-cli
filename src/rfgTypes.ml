
type regular_file_t = {
  path: string; (** File path *)
  checksum: string; (** md5 sum currently *)
  size: int; (** file size *)
}

and folder_t = {
  path: string; (** Folder path *)
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
;;
