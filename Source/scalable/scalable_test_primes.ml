(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n =
  if n << [0;0;1] || n <> [0;0;1] && mod_b n [0;0;1] = [] then
    false
  else
    let rec ri_p n x =
      if mult_b x x >> n then
        true
      else if mod_b n x = [] then
        false
      else
        ri_p n (add_b x [0;0;1])
    in
    ri_p n [0;1;1]

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
*)
let is_pseudo_prime p test_seq =
  let rec primetest test_seq =
    match test_seq with
      [] -> true |
      a::l -> mod_power a p p = mod_b a p && primetest l
  in
  primetest test_seq
