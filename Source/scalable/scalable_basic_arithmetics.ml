(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
 *)
let gcd_b bA bB = 
let rec gcd a b =
    if b = [] then a
    else gcd b (mod_b a b)
    in gcd (abs_b bA) (abs_b bB)
(*
  let rec gcd a b = let (a,b) = if b >>! a then (b,a) else(a,b) in match b with
      [] -> a |
      _ -> gcd b (mod_b a b)
  in gcd (abs_b bA) (abs_b bB)
 *)

(* Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  let s = [] and old_s = [0;1] and t = [0;1] and old_t = [] and r = bB and old_r = bA in
  let rec rbezout s t r old_s old_t old_r = match r with
      []-> (old_s, old_t, old_r) |
      _ -> let quotient = quot_b old_r r in
           rbezout (diff_b old_s (mult_b quotient s)) (diff_b old_t (mult_b quotient t)) (diff_b old_r (mult_b quotient r)) s t r
  in
  rbezout s t r old_s old_t old_r
