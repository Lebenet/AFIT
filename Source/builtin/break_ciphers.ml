(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)

let break key =
  let rec rb k nb =
    if modulo k nb == 0 then
      (nb, quot k nb) else
      rb k (nb-1)
  in
   let (key, _) = key in
   let root = int_of_float (floor (sqrt (float_of_int key))) in
   if modulo root 2 == 1 then
      rb key (root-1)
   else
      rb key root
