(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let rec rb k nb =
    if equal (erem k nb) zero then
      (nb, ediv k nb) else
      rb k (sub nb one)
  in
   let (key, _) = key in
   let root = sqrt key in
   if equal (erem root (succ one)) one then
      rb key (sub root one)
   else
      rb key root
