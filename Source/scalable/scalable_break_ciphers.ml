(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

let modulo a b = let res = a mod b in if res < 0 then if b < 0 then res - b else res + b else res
let quot a b = (a - modulo a b) / b

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)

(*
let break_int key =
  let rec rb k nb =
    if modulo k nb = 0 then
      (from_int nb, from_int (quot k nb)) else
      rb k (nb - 2)
  in
   let (key, _) = key in
   let key = to_int key in
   let root = int_of_float (floor (sqrt (float_of_int key))) in
   if modulo root 2 = 1 then
      rb key (root - 2)
   else
      rb key root
*)

let break key = 
  let (k,_) = key in
  if mod_b k [0;0;1] = [] then
    (quot_b k [0;0;1], [0;0;1]) else
  let rec rb nb =
    if mod_b k nb = [] then
      (nb, quot_b k nb)
    else
      rb (add_b nb [0;0;1])
  in
    rb [0;1;1]
(*
  if mod_b a [0;0;1] = [] then
    (quot_b a [0;0;1], [0;0;1])
  else if a >> [0; 0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
 1; 1; 1; 1; 1; 1; 1] then ([],[])
  else
    break_int key
*)
