(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n = match n with
    [] -> [0; 1] |
    [0;1] -> x |
    n when n << [] -> invalid_arg "negative n" |
    _ -> let rec aux n =
           match n with
             [] -> [0; 1] |
             _ -> mult_b x (aux (diff_b n [0;1]))
         in aux n

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n = match n with
    n when n << [] -> invalid_arg "n negative" |
    []-> [0;1] |
    _ -> let rec rpower n = match n with
             [] -> [0;1] |
             [0;1]-> x |
             [0;0;1] -> mult_b x x |
             n when mod_b n [0;0;1] = [] -> let p = rpower (quot_b n [0;0;1]) in mult_b p p |
             _ -> let p = rpower (quot_b (diff_b n [0;1]) [0;0;1]) in mult_b (mult_b p p) x
         in
         rpower n

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
(*
let rec pow2 n = match n with
    [0;1] -> true |
    n when mod_b n [0;0;1] != [] -> false |
    _ -> pow2 (quot_b n [0;0;1])*)

(*
let mod_power x n m =
  if n << [] then invalid_arg "n negative" else
    let rec rmod_power x n = match n with
        [0;1] -> mod_b x m |
        n when mod_b n [0;0;1] = [] -> let p = rmod_power (mod_b x m) (quot_b n [0;0;1]) in mod_b (mult_b p p) m |
        _ -> let p = rmod_power (mod_b x m) (quot_b n [0;0;1]) in mod_b (mult_b (mult_b p p) (mod_b x m)) m
    in
    match n with
      [] -> [0;1] |
      n when pow2 n -> rmod_power x n |
      _ -> let rec exponentMod x n m =
             if x = [] then
               []
             else if n = [] then
               [0;1]
             else if mod_b n [0;0;1] = [] then
               let y = exponentMod x (quot_b n [0;0;1]) m in
               let res = mod_b (mult_b y y) m in
               mod_b (add_b res m) m
             else
               let y = mod_b x m in
               let res = mod_b (mult_b y (mod_b (exponentMod x (diff_b n [0;1]) m) m)) m in
               mod_b (add_b res m) m
           in
           exponentMod x n m*)

let mod_power x n m =
  let rec mp = function
      (_, _, [0;1]) -> [] |
      (_, [], _) -> [0;1] |
      (x, n, m) when mod_b n [0;0;1] = [] -> let h = mp (x, (quot_b n [0;0;1]), m) in mod_b (mult_b h h) m |
      (x, n, m) -> mod_b (mult_b x (mp (x, (diff_b n [0;1]), m))) m
  in
  let res = mp (x, n, m) in if res << [] then mod_b (add_b m res) m else res

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  if n << [] then invalid_arg "n negative" else
    if n = [] then [0;1] else
      if x = [] then [] else
        let (q,r) = div_b n (diff_b p [0;1]) in
        mod_power x r p
