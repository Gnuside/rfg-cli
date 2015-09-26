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
  print_endline (Printf.sprintf
    "FileGenerator.create min_size:%d max_size:%d filename:%s path:%s"
    mis mas fn p);
  (match size with None -> print_endline "size:None" | Some(s) -> print_endline (Printf.sprintf "size:%d" s));
  let buf_size = 1024
  and file_path = (p ^ "/" ^ fn) in
  let buffer = Bytes.create buf_size
  and current_size = ref 0
  and oc = open_out_bin file_path
  and s = match size with None -> mis + (Random.int (mas-mis)) | Some(s) -> s
  in
  while !current_size < s do
    for i = 0 to buf_size - 1 do
      Bytes.set buffer i (char_of_int (Random.int 256))
    done;
    (* TODO: optimize for speed by cumulating small buffers into a big one,
     * Currently we just let the filesystem and flush system to optimize for us *)
    incr current_size;
    output_bytes oc buffer
  done;
  close_out oc;
  RegularFile{
    filepath = file_path;
    size = s;
    checksum = Digest.file file_path (* FIXME: fucking not optimal. We just had the data in hand please Digest at the same time *)
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

