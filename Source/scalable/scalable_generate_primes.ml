(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  if n << [0;0;1] then
    invalid_arg "n too small"
  else
    let rec aux i =
      if i >> n then
        []
      else
        i::(aux (add_b i [0;0;1]))
    in
    [0;0;1]::(aux [0;1;1])

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let rec remove_mult x l =
  match l with
    [] -> [] |
    e::l2 ->
      if mod_b e x = [] then
        remove_mult x l2
      else
        e::(remove_mult x l2)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  if n << [0;0;1] then
    invalid_arg "n too smol"
  else
    let list = init_eratosthenes n in
    let rec supp l =
      match l with
        [] -> [] |
        e::l ->
          if mult_b e e >> n then e::l
          else
            e::(supp (remove_mult e l))
    in
    supp list

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = 
  let f = open_out file in
  let rec get_l = function
    [] -> "" |
    e::l -> (string_of_int e) ^ "," ^ (get_l l) 
  in 
  let rec write l = match l with
    [] -> flush f; |
    e::l -> begin output_string f ((get_l e) ^ "\n"); write l end
  in 
    begin
      write li;
      close_out f;
    end


(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (from_int (int_of_string line))::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = create_list (open_in file)

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
  let rec gen_double l = match l with
      [] -> [] |
      e::l -> let k = add_b (mult_b e [0;0;1]) [0;1] in if isprime k then (e, k)::gen_double l else gen_double l in
  gen_double (eratosthenes limit)

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = 
  let rec gen_double l = match l with
      [] -> [] |
      e::l -> let k = add_b e [0;0;1] in if isprime k then (e, k)::gen_double l else gen_double l in
  gen_double (eratosthenes limit)
