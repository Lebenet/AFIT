(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let modulo a b =
  let res = a mod b in
  if res < 0 then
    if b < 0 then
      res - b
    else
      res + b
  else
    res

let quot a b = (a - modulo a b) / b


let break key = let (k,_) = key in
  let rec rb k nb =
    if modulo k nb = 0 then
      (nb, quot k nb) else
      rb k (nb-1)
  in
   let k = to_int k in
   let root = int_of_float (floor (sqrt (float_of_int k))) in
   if modulo root 2 = 1 then
      let (a,b) = (rb k (root-1)) in (from_int a, from_int b)
   else
      let (a,b) = (rb k root) in (from_int a, from_int b)
