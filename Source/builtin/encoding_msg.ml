(** Encoding Strings *)

(*open Builtin
open Basic_arithmetics
open Power*)

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = String.length str in
  let rec rencode i c =
    if i == len then
      c
    else
      rencode (i+1) (c lsl bits + (Char.code str.[i]))
  in
  rencode 0 0

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)

(* auxiliary function to get bits-rightest bits from n *)
let getbits n bits = ((n lsl (Sys.int_size - bits)) lsr (Sys.int_size - bits), n lsr bits)

let decode msg bits =
  let rec rdecode = function
      0 -> "" |
      n -> let (letter, newn) = getbits n bits in (rdecode newn) ^ (Char.escaped (Char.chr letter))
  in
  rdecode msg
