(** Generating primes *)

open Z

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
   A light version done in-class.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n upper limit of elements in the list of big integers.
 *)
let two = of_int 2
let three = of_int 3

let init_eratosthenes n =
  if lt n two then
    invalid_arg "n too small"
  else
    let rec aux i =
      if gt i n then
        []
      else
        i::(aux (add i two))
    in
    two::(aux three)

(* Eratosthenes sieve. *)

(** removes multiples of x in list l *)
let rec remove_mult x l =
  match l with
    [] -> [] |
    e::l2 ->
     if (equal (erem e x) zero) && (not (equal e x)) then
       remove_mult x l2
     else
       e::(remove_mult x l2)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
 *)
let eratosthenes n =
  if lt n two then
    invalid_arg "n too smol"
  else
    let list = init_eratosthenes n in
    let rec supp l =
      match l with
        [] -> [] |
        e::l ->
         if gt (mul e e) n then e::l
         else
           e::(supp (remove_mult e l))
    in
    supp list

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let f = open_out file in
  let rec rwl li = match li with
      [] -> flush f |
      e::l ->
       begin
          output_string f ("\n" ^ to_string e);
          rwl l;
       end
  in
    match li with
      [] -> close_out f |
      e::l ->
       begin
         output_string f (to_string e);
         rwl l;
         close_out f;
       end

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = create_list (open_in file)

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(* Generating couples of primes numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive big integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec gen_double l = match l with
      [] -> [] |
      e::l -> if isprime (add (mul e two) one) then (e, add (mul e two) one)::gen_double l else gen_double l in
  gen_double (eratosthenes limit)
