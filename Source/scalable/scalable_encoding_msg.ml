(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = String.length str in
  let rec rencode i c =
    if i = len then
      c
    else
      rencode (i+1) (add_b (shift c bits) (from_int (Char.code str.[i])))
  in
  rencode 0 []

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let getbits n bits =
  let rec rg n bits = if bits = 0 then [] else
      match n with 
        [] -> [] |
        e::l -> e::(rg l (bits-1))
  and
    rm n bits = if bits = 0 then n else
      match n with
        [] -> n |
        e::l -> rm l (bits-1)
  in
  (rg n bits, rm n bits)
      

let decode msg bits =
  let rec rdecode = function
      [] -> "" |
      n -> let (letter, newn) = getbits n bits in (rdecode newn) ^ (Char.escaped (Char.chr (to_int (0::letter))))
  in
  let (n, newn) = getbits msg (bits+1) in (rdecode newn) ^ (Char.escaped (Char.chr (to_int n)))
