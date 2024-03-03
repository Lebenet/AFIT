(** Basic arithmetics with builtin integers *)
open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b = match b with
    0 -> a |
    _ -> gcd b (modulo a b)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let s = 0 and old_s = 1 and t = 1 and old_t = 0 and r = b and old_r = a in
  let rec rbezout s t r old_s old_t old_r = match r with
      0 -> (old_s, old_t, old_r) |
      _ -> let quotient = quot old_r r in
           rbezout (old_s - quotient*s) (old_t - quotient*t) (old_r - quotient*r) s t r
  in
  rbezout s t r old_s old_t old_r
