(** Encoding Strings *)

open Z
open Z_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = String.length str in
  let rec rencode i c =
    if (to_int i) = len then
      c
    else
      rencode (add i one) (add (shift_left c bits) (of_int (Char.code (str.[(to_int i)]))))
  in
  rencode zero zero

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
(*
let getbits n bits = let s = of_int (size n) in let decal = (if gt (sub s (of_int bits)) zero then sub s (of_int bits) else zero) in (shift_right (shift_left n (to_int decal)) (to_int decal), shift_right n bits)
*)

let getbits n bits = (logand n (sub (shift_left one bits) one), shift_right n bits)

let decode msg bits =
  let rec rdecode x =
      if x = zero then ""
      else let (letter, newn) = getbits x bits in (rdecode newn) ^ (Char.escaped (Char.chr (to_int letter)))
  in
  rdecode msg
