open Random
open Arg

let seed = ref None;;

let args = [
  ("--seed", Int(fun s -> seed := Some(s)), "Set initial seed for Random function (by default use /dev/urandom if available).")
];;

let anon_fun arg =
  failwith (Printf.sprintf "No anonyme argument please : '%s'." arg)
;;

let usage_msg =
  Printf.sprintf "%s: TODO put a message here." Sys.executable_name
;;

Arg.parse args anon_fun usage_msg;;

let _ =
  (match !seed with
  None -> Random.self_init ()
  | Some(s) -> Random.init s
  );
  print_endline (
    Printf.sprintf "Here is the first random number (from 0 to 100) got: %d" (Random.int 100)
  )
;;
