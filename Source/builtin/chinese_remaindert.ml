(** Chinese remainder theorem *)

open Builtin
open Basic_arithmetics

(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)
let crt_image x l =
  let rec aux li = match li with
      [] -> [] |
      e::l -> (modulo x e)::(aux l)
  in
  aux l

(** Inverse image of Chinese Remainder map
    @para m a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)
let crt_solver m l y =
  let rec prod = function
      [] -> 1 |
      e::l -> e * (prod l)
  in
  let n = prod l in
  let rec getMs = function
      [] -> [] |
      e::l -> (quot n e)::(getMs l)
  in
  let ms = getMs l in
  let rec getInvMs = function
      ([], []) -> [] |
      (a::l1, b::l2) -> let (c, _, _) = bezout (modulo a b) b in c::(getInvMs (l1, l2)) |
      _ -> failwith "tmr"
  in
  let ys = getInvMs (ms, l) in
  let rec sum = function
      ([], [], []) -> 0 |
      (a::l1, b::l2, c::l3) -> a*b*c + (sum (l1, l2, l3)) |
      _ -> failwith "tmr"
  in
  modulo (sum (ys, y, ms)) m
