(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(*
#use "builtin.ml";;
#use "basic_arithmetics.ml";;
#use "power.ml";;
 *)

(** Deterministic primality test *)
let is_prime n =
  if n < 2 || n != 2 && n mod 2 == 0 then
    false
  else
    let rec ri_p n x =
      if x * x > n then
        true
      else if n mod x == 0 then
        false
      else
        ri_p n (x + 2)
    in
    ri_p n 3

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)

let is_pseudo_prime p test_seq =
  let rec primetest test_seq =
  match test_seq with
    [] -> true |
    a::l -> mod_power a p p == a mod p && primetest l
  in
  primetest test_seq


