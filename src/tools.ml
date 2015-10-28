(** This is an utility module *)


(* Random facility *)

let random_ic = (*if Sys.file_exists "/dev/urandom" then Some(open_in_bin "/dev/urandom") else *)None;;

open B64
open Filename

(** Get a base64 random string *)
let random_b64_string l =
  let res = Bytes.create l in
  for i = 0 to l-1 do
    Bytes.set res i (char_of_int (Random.int 256))
  done; (* Make the random bytes *)
  encode ~alphabet:(uri_safe_alphabet) (Bytes.to_string res)
;;

let hex_string s = match Hex.of_string s with `Hex (s) -> s;;

(** Give a random number between imin and imax, except that it will take the max
 * between 1 and max of imin and (imax - imin) *)
let get_random imin imax =
  let i = max 1 (max imin (imax - imin)) in
  (*print_endline (Printf.sprintf "get_random: %d to %d (%d)" imin imax i);*)
  let res = imin + (
    if i > 1073741823 then
      Int64.to_int (Random.int64 (Int64.of_int i))
    else Random.int i
      ) in
  (*print_endline (Printf.sprintf "get_random: got %d" res);*)
  res
;;

(* This is a fake random, because generating a real random serie of bytes is too slow. *)
let random_buffer_size = 5*1024*1024;;
let random_buffer = Bytes.create random_buffer_size;;
let _ =
  for i = 0 to random_buffer_size - 1 do
    Bytes.set random_buffer i (char_of_int (Random.int 256))
  done

;;

let get_bytes_random len =
  let rand_i = Random.int random_buffer_size
  and buffer = Bytes.create len in
  Bytes.blit random_buffer rand_i buffer 0 (min len (random_buffer_size - rand_i));
  (if len > (random_buffer_size - rand_i) then
    Bytes.blit random_buffer 0 buffer (random_buffer_size - rand_i) (len - (random_buffer_size - rand_i))
  );
  buffer
;;


(* Status facility *)
open Printf
let refresh_rate = 1.000 (* seconds *)
and max_path_width = 40
and global_cur_files = ref 0
and global_cur_amount = ref 0
and max_files = ref 0
and max_amount = ref 0
and cur_file_amount = ref 0
and cur_file_max = ref 0
and cur_file = ref 0
and cur_max_file = ref 0 (* current folder maximum number of files *)
and cur_path = ref ""
and cur_kind = ref ""
and cur_depth = ref 0
and mutex = Mutex.create ()
and stop = ref false
and thread = ref (Thread.self ())
and thread_start = ref 0.0
;;

(** refresh the status if the current element
 * - @kind : "file" or "folder"
 * - @path : path of the file
 * - @cfm : current file max size (size of file creating, or current max fixed size for folders)
 * - @diff : amount of data create since last refresh
 * - @cnf : current number of file (for folders)
 * - @cmf : current max number of files (for folders)
 * - @depth : tree depth
 * *)
let refresh_status path cfm diff ?cnf ?cmf ?depth kind =
  Mutex.lock mutex;
  begin
    cur_kind := kind;
    cur_file_max := cfm;
    match kind with
  | "file" -> begin
    (if !cur_path != path then begin
      incr global_cur_files;
      cur_path := path;
      cur_file_amount := 0
    end);
    global_cur_amount := !global_cur_amount + diff;
    cur_file_amount := !cur_file_amount + diff;
    end
  | "folder" -> begin
    cur_path := path;
    cur_file_amount := diff;
    (match cnf with
    | None -> failwith "Specify cnf current number of file in folder refresh"
    | Some(_cnf) -> cur_file := _cnf);
    (match cmf with
    | None -> failwith "Specify cmf current max number of file in folder refresh"
    | Some(_cmf) -> cur_max_file := _cmf);
    (match depth with
    | None -> failwith "Specify current depth in folder refresh"
    | Some(_depth) -> cur_depth := _depth);
  end
  | k -> failwith (sprintf "refresh_status - umanaged kind: %s" k)
  end;
  Mutex.unlock mutex;
  Thread.yield () (* Could schedule now *)
;;

let stop_status () =
  stop := true;
  Thread.join !thread
;;

let status_shorten_path path =
  let len = String.length path in
  if len < max_path_width then
    path
  else
    (String.sub path 0 18) ^ "..." ^ (String.sub path (len-19) 19)
;;

let display_status () =
  Mutex.lock mutex;
  let elapsed_time = (Unix.gettimeofday ()) -. !thread_start
  and path_shorten = status_shorten_path !cur_path in
  let rate = ((float_of_int !global_cur_amount) /. elapsed_time) /. 1024. in
  printf "\r%.03fs (%3.1f MB/s) -%3d- %s" elapsed_time rate !cur_depth path_shorten;
  (match !cur_kind with
  | "file" -> printf " - file %11d / %d KB" !cur_file_amount !cur_file_max
  | "folder" -> printf " - folder %11d / %d KB | %6d / %d files"
    !cur_file_amount !cur_file_max
    !cur_file !cur_max_file
  | _ -> printf " - Unknown");
  printf " - Global %11d / %d KB | %6d / %d files"
    !global_cur_amount !max_amount
    !global_cur_files !max_files;
  flush stdout;
  Mutex.unlock mutex
;;

let refresh_thread () =
  thread_start := Unix.gettimeofday ();
  while true do
    Thread.delay refresh_rate;
    display_status ();
    if !stop then Thread.exit ()
  done
;;

let init_status mf ma file_max_size =
  Mutex.lock mutex;
  max_files := mf;
  max_amount := ma;
  print_endline (sprintf "Creating %d files with a max size of %d KB for an amount of %d KB." mf file_max_size ma);
  Mutex.unlock mutex;
  thread := Thread.create refresh_thread ()
;;

let remove_trailing_slash str =
  if check_suffix str dir_sep then
    chop_suffix str dir_sep
  else
    str
;;

let checksum str =
  hex_string (Digest.file str)
;;
