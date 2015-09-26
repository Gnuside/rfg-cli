(** This is an utility module *)

open B64

(** Get a base64 random string *)
let random_b64_string l =
  let res = Bytes.create l in
  for i = 0 to l-1 do
    Bytes.set res i (char_of_int (Random.int 256))
  done; (* Make the random bytes *)
  encode ~alphabet:(uri_safe_alphabet) (Bytes.to_string res)
;;

let hex_string s = match Hex.of_string s with `Hex (s) -> s;;
