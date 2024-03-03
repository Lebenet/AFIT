(** Power function implementations for builtin *)
(*#use "builtin.ml";;
#use "basic_arithmetics.ml";;*)
open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n = match n with
    n when n < 0 -> invalid_arg "n negative" |
    0 -> 1 |
    _ -> let rec rpow x n = match n with
             1 -> x |
             _ -> x * rpow x (n-1)
         in
         rpow x n

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n = match n with
    n when n < 0 -> invalid_arg "n negative" |
    0 -> 1 |
    _ -> let rec rpower x n = match n with
             1 -> x |
             n when modulo n 2 == 0 -> let p = rpower x (n/2) in p*p |
             _ -> let p = rpower x ((n-1)/2) in p*p*x
         in
         rpower x n

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)

(** func to check whether or not n is a power of 2 *)
let rec pow2 n = match n with
    1 -> true |
    n when modulo n 2 != 0 -> false |
    _ -> pow2 (n/2)


let mod_power x n m =
  if n < 0 then invalid_arg "n negative" else
  let rec rmod_power x n = match n with
      1 -> modulo x m |
      n when modulo n 2 == 0 -> let p = rmod_power (modulo x m) (n / 2) in modulo (p*p) m |
      _ -> let p = rmod_power (modulo x m) (n / 2) in modulo (p*p*(modulo x m)) m
  in
  match n with
    0 -> 1 |
    n when pow2 n -> rmod_power x n |
    _ -> let rec exponentMod x n m =
             if x == 0 then
               0
             else if n == 0 then
               1
             else if modulo n 2 == 0 then
               let y = exponentMod x (n/2) m in
               let res = modulo (y * y) m in
               modulo (res + m) m
             else
               let y = modulo x m in
               let res = modulo (y * modulo (exponentMod x (n-1) m) m) m in
               modulo (res + m) m
         in
         exponentMod x n m


(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if n < 0 then invalid_arg "n negative" else
    if n == 0 then 1 else
      if x == 0 then 0 else
        let (q,r) = div n (p-1) in
        mod_power x r p
