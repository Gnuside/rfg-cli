(** This module is in charge of creating files
 *)

open RfgTypes
open Tools

(** Create a random name for a file :
 * - @base_name is the base of the name used (ie new_name ?base_name:'file-' will generate a name like 'file-34df56')
 * - @length is the length of the random part (8 by default)
 * - @end_name is the extension name (none by default, add '.' if you want an extension)
 * *)
let new_name ?length:(l=8) ?base_name:(b="file-") ?end_name:(e="") () =
  b ^ (random_b64_string l) ^ e
;;

(** Create a random file and fill its content with random data
 * - @size is the size of the file in KB (by default random from 5 to 5 GB (5*1024*1024))
 *    - If @size not set, @min_size and @max_size are took as boundaries for a random size.
 * - @filename is the name of the file (by default it is made by new_name ())
 * This returns an RfgTypes.file_t :
   * - filepath
   * - md5sum
   * - file size
 *)
let create ?min_size:(mis=5) ?max_size:(mas=5242880) ?size ?filename:(fn=new_name ()) ?path:(p=".") () =
  (*print_endline (Printf.sprintf
    "FileGenerator.create min_size:%d max_size:%d filename:%s path:%s"
    mis mas fn p);*)
  (*(match size with None -> print_endline "size:None" | Some(s) -> print_endline (Printf.sprintf "size:%d" s));*)
  let buf_size = 1024
  and file_path = (p ^ "/" ^ fn) in
  let current_size = ref 0
  and oc = open_out_bin file_path
  and (md5_ic, md5_oc) = Unix.open_process "md5sum"
  and s = match size with None -> get_random mis (max 1 (mas-mis)) | Some(s) -> s
  in
  Tools.refresh_status file_path s 0 "file";
  while !current_size < s do
    let s_used = min (s - !current_size) 2048 in
    let buffer = get_bytes_random (s_used*buf_size) in
    current_size := !current_size + s_used;
    (* output to the file and the md5 channel to md5sum process *)
    output oc buffer 0 (s_used*buf_size);
    output md5_oc buffer 0 (s_used*buf_size);
    flush md5_oc; (* don't forget to flush, else it is kept in the pipe *)
    Tools.refresh_status file_path s s_used "file";
  done;
  close_out oc;
  (* close input of md5sum to make it finish its job *)
  close_out md5_oc;
  (* get the displayed md5 sum (only the first line normally... *)
  let md5_string = input_line md5_ic in
  ignore (Unix.close_process (md5_ic, md5_oc));
  RegularFile{
    filepath = file_path;
    size = s;
    checksum = String.sub md5_string 0 32
  }
;;

let print_file_t_file level (f:regular_file_t) =
  let space_level = String.make (level*2) ' ' in
  let print_indent () = print_string space_level in
  print_indent (); print_endline "File";
  print_indent (); print_endline ("path: " ^ f.filepath);
  print_indent (); print_endline (Printf.sprintf "size: %d KB" f.size);
  print_indent (); print_endline ("checksum: " ^ (hex_string f.checksum))
;;

