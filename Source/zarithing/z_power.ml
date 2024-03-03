(** Power function implementations for big integers *)

open Z

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec compute x n m = 
      if equal m one then zero
      else if equal n zero then one
      else if equal (erem n (of_int 2)) zero then let h = compute x (ediv n (of_int 2)) m in erem (mul h h) m
      else erem (mul x (compute x (sub n one) m)) m
  in
  let res = compute x n m in if lt res zero then erem (add m res) m else res

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)


let prime_mod_power x n p =
  if lt n zero then invalid_arg "n negative" else
    if equal n zero then one else
      if equal x zero then zero else
        let (q,r) = ediv_rem n (sub p one) in
        mod_power x r p