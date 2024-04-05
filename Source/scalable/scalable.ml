 (** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

  *)

let rec reverse l1 l2 = match l1 with
    [] -> l2 |
    e::l -> reverse l (e::l2)

let remove_zeros l =
  let r = List.rev l in
  let rec rr l = match l with
      [] ->  [] |
      0::l -> rr l |
      e::l -> e::l
  in
  List.rev (rr r)

let abs x = if x < 0 then (-x) else x

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  if x = 0 then [] else
  let rec aux a =
    if a = 0 then
      []
    else
      (a mod 2)::(aux (a lsr 1))
  in
  (if x < 0 then 1 else 0)::(aux (abs x))

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA = match bA with
    [] -> 0 |
    sign::l ->
     let rec transform i = function
         [] -> 0 |
         e::l -> e*i + (transform (i lsl 1) l)
     in
     (transform 1 l) * (if sign = 1 then (-1) else 1)

(* Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec rp = function
      [] -> "" |
      e::l -> (rp l)^(string_of_int e)
  in
  match bA with
    [] -> print_string("0") |
    sign::l -> print_string((if sign = 0 then "" else "-")^(rp l))

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)

let rec equalN l1 l2 =
  match (l1, l2) with
    ([], []) -> true |
    ([], _) | (_, []) -> false |
    (a::l1, b::l2) -> a==b && equalN l1 l2
(*
let rec compare_n lA lB =
  match (lA, lB) with
    ([], _::_) | ([], []) -> -1 |
    (_::_, []) -> 1 |
    (a::l1, b::l2) -> if (a == b) then compare_n l1 l2 else if a == 1 && b == 0 then 1 else -1*)

let rec compare_n nA nB =
  let lenA = List.length nA and lenB = List.length nB in
  if lenA < lenB then -1 else if lenA > lenB then 1 else
    let rec compare lA lB =
      match (lA, lB) with
        ([], []) | (_, _) when nA = nB -> 0 | ([], _)-> (-1) |
        (_, []) -> 1 |
        (a::l1, b::l2) -> if (a == b) then compare l1 l2 else if a > b then 1 else (-1)
    in
    compare (reverse nA []) (reverse nB [])

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = compare_n nA nB = 1

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = compare_n nA nB = (-1) && not (equalN nA nB)

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = not (nA <<! nB)

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = not (nA >>! nB)

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
 *)
(*
let compare_b bA bB = match (bA, bB) with
    ([], []) -> (-1) | ([], e::_) when e = 0 -> (-1) | (e::_, []) when e = 1 -> (-1) | (e::_, []) when e = 0 -> 1 | ([], e::_) when e = 1 -> 1 | ([], _::_) | (_::_, []) -> failwith "impossible to get there" |
    (a::l1, b::l2) ->
     let (lA, lB) = (List.length bA, List.length bB) in
     match (a, b) with
       (1, 0) -> (-1) | (0, 1) -> 1 |
       (0, 0) ->
        if lA > lB then 1 else if lA < lB then (-1) else
          let rec rcb l1 l2 = match (l1, l2) with
              ([], []) -> (-1) |
              (a::l1, b::l2) -> if a = b then rcb l1 l2 else
                                  if a = 1 && b = 0 then 1 else (-1) |
              _ -> failwith "not possible to get there"
          in
          rcb l1 l2 |
       (1, 1) ->
        if lA > lB then (-1) else if lA < lB then 1 else
          let rec rcb l1 l2 = match (l1, l2) with
              ([], []) -> (-1) |
              (a::l1, b::l2) -> if a = b then rcb l1 l2 else
                                  if a = 1 && b = 0 then (-1) else 1 |
              _ -> failwith "not possible to get there"
          in
          rcb l1 l2 |
       _ -> failwith "not possible to get there"
 *)

let compare_b bA bB = match (bA, bB) with
    ([], []) -> 0 |
    (_, _) when bA = bB -> 0 |
    ([], e::l) -> if e = 1 then 1 else (-1) |
    (e::l, []) -> if e = 1 then (-1) else 1 |
    (a::l1, b::l2) -> if a = 1 && b = 1 then -(compare_n l1 l2)
                      else if a > b then (-1)
                      else if a < b then 1
                      else compare_n l1 l2

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = compare_b bA bB = 1

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = compare_b bA bB = (-1) && (match (bA, bB) with
                                          (a::l1, b::l2) -> not (equalN l1 l2) | _ -> true)

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = not (bA << bB)

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = not (bA >> bB)


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
    [] -> 1 |
    e::l -> if e = 1 then (-1) else 1

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match sign_b bA with
    1 -> bA |
    _ -> match bA with
              [] -> [] |
              e::l -> 0::l

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  match (nA, nB) with
    ([], []) -> [] |
    (_, []) -> nA |
    ([], _) -> nB |
    _ -> let rec add r a b =
           match (a, b) with
             ([], []) -> [r] |
             (e::l, []) -> ((e+r) mod 2)::(add (if e+r = 2 then 1 else 0) l []) |
             ([], e::l) -> ((e+r) mod 2)::(add (if e+r = 2 then 1 else 0) [] l) |
             (e1::l1, e2::l2) -> let q = e1+e2+r in (q mod 2)::(add (if q >= 2 then 1 else 0) l1 l2)
      in remove_zeros (add 0 nA nB)

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let rec rd lA lB borrow =
    match (lA, lB) with
      ([], _) -> [] |
      (e::l, []) -> if borrow = 0 then e::(rd l [] 0)
                    else
                      if e = 1 then 0::(rd l [] 0)
                      else
                        1::(rd l [] 1) |
      (a::l1, b::l2) ->       
        (*(if a = b then borrow else 1 - borrow)::(rd l1 l2 (if a >= b then borrow else 1 - borrow))
        *)
        let aux a b borrow =
          if borrow = 0 then
            match (a, b) with
              (0,0) | (1,1) -> 0::(rd l1 l2 0) |
              (1,0) -> 1::(rd l1 l2 0) |
              (0,1) -> 1::(rd l1 l2 1) |
              _ -> failwith "unused"
          else
            match (a,b) with
              (1, 0) -> 0::(rd l1 l2 0) |
              (1, 1) -> 1::(rd l1 l2 1) |
              (0, 1) -> 0::(rd l1 l2 1) |
              (0, 0) -> 1::(rd l1 l2 1) |
              _ -> failwith "unused"
        in
        aux a b borrow
  in
  remove_zeros (rd nA nB 0)

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = match (bA, bB) with
    ([], []) -> [] |
    ([], _) -> bB |
    (_, []) -> bA |
    (a::l1, b::l2) ->
  match (sign_b bA, sign_b bB) with
    (1, 1) -> 0::(add_n l1 l2) |
    (-1, 1) -> if equalN l1 l2 then [] else
                 (if 0::l1 >> 0::l2 then 1 else 0)::(if 0::l1 >> 0::l2 then diff_n l1 l2 else diff_n l2 l1) |
    (1, -1) -> if equalN l1 l2 then [] else
                 (if 0::l1 >> 0::l2 then 0 else 1)::(if 0::l1 >> 0::l2 then diff_n l1 l2 else diff_n l2 l1) |
    (-1, -1) -> 1::(add_n l1 l2) |
    _ -> failwith "unused"

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =  match (bA, bB) with
    ([], []) -> [] |
    ([], e::l) -> if e = 1 then 0::l else 1::l |
    (_, []) -> bA |
    (a::l1, b::l2) ->
     match (sign_b bA, sign_b bB) with
       (1,1) -> add_b bA ((if b = 1 then 0 else 1)::l2) |
       (1,-1) -> 0::(add_n l1 l2) |
       (-1, 1) -> 1::(add_n l1 l2) |
       (-1, -1) -> add_b bA (abs_b bB) |
       _ -> failwith "unused"

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d =
  match bA with
    [] -> [] |
    sign::l ->
     let rec rs d l =
       if d = 0 then
         match l with
           [] -> [] |
           e::l -> e::(rs d l)
       else
         0::(rs (d-1) l)
  in
  sign::(rs d l)

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  let rec addzeros l c =
    if c = 0 then l
    else
      0::(addzeros l (c-1))
  in
  let rec sum l = match l with
      [] -> [] |
      [e] -> e |
      e1::e2::l -> sum ((add_b e1 e2)::l)
  in
  match (bA, bB) with
    ([], []) | ([], _) | (_, []) -> [] |
    (a::l1, b::l2) ->
      let rec aux l2 c = match l2 with
          [] -> [] |
          e::l2 -> if e = 1 then (a::(addzeros l1 c))::(aux l2 (c+1)) else aux l2 (c+1)
      in
      match remove_zeros (sum (aux l2 0)) with
        [] -> [] |
        e::l -> (match (sign_b bA, sign_b bB) with ((-1),1) | (1,(-1)) -> 1 | _ -> 0)::l

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
(*let rec euclidean_binary_division_quotient dividend divisor acc =
  if dividend < divisor then
    acc
  else
    let quotient = shift acc 1 in
    let new_dividend = diff_b dividend divisor in
    if new_dividend >>= divisor then
      euclidean_binary_division_quotient new_dividend divisor (add_b quotient [0;1])
    else
      quotient

let quot_b bA bB =
  if bA < bB then
    []
  else
    let q = [0;1] in
    euclidean_binary_division_quotient (diff_b bA bB) bB q*)

let rec getFirstsOfA n a res =
  if n = 0 then (res, a)
  else match a with
      e::l -> getFirstsOfA (n-1) l (e::res) |
      _ -> failwith "weird"

let extBuff1 buffer nA =
  match nA with
    [] -> (buffer, []) |
    e::l -> if buffer = [] && e = 0 then (buffer, l) else (e::buffer, l)
    
            
let build_list_str li =
  let rec main l = match l with
      [] -> "]" |
      [e] -> string_of_int e ^ "]" |
      e::l -> (string_of_int e) ^ "; " ^ (main l)
  in
  "[" ^ main li

(*
let debug_quot_n nA nB =
  if nA <<! nB then []
  else if equalN nA nB then [1]
  else 
    let b = List.rev nB and a = List.rev nA in
    let (initBuffer, initA) = getFirstsOfA (List.length b) a [] in
    let rec theDivision a buffer = 
      let () = Printf.printf "\ncomparison: buffer = %s, divisor = %s \nbuffer greater than or equal than divisor: %b\n" (build_list_str (List.rev buffer)) (build_list_str b) (buffer >=! nB) in
      if buffer <<! nB then
        let () = Printf.printf "substraction not possible, adding 0 to output" in
        if a = [] then let () = Printf.printf "\ndividend empty, returning output\n" in [0] else
          let (newBuffer, newA) = extBuff1 buffer a in 
          let () = Printf.printf "\nextending buffer:\n new buffer = %s , new a = %s\n" (build_list_str (List.rev newBuffer)) (build_list_str newA) in 
          0::(theDivision newA newBuffer)
      else
        let newBuff = diff_n buffer nB in
        let () = Printf.printf "\nsubstraction possible, sub: %s - %s = %s \nadding 1 to output \nnew buffer = result = %s\n" (build_list_str (List.rev buffer)) (build_list_str b) (build_list_str (List.rev newBuff)) (build_list_str (List.rev newBuff)) in 
        if a = [] then let () = Printf.printf "\ndividend empty, returning output\n" in [1] else
          let (newBuffer, newA) = extBuff1 newBuff a in
          let () = Printf.printf " extending buffer: new buffer = %s , new a = %s\n" (build_list_str (List.rev newBuffer)) (build_list_str newA) in 
          1::(theDivision newA newBuffer)
    in
    let (initBuffer, initA) = if initBuffer <<! nB then extBuff1 initBuffer initA else (initBuffer, initA) in
    let () = Printf.printf "init: \nbuffer = first %s of a: %s and new a = %s\n" (string_of_int (List.length initBuffer)) (build_list_str (List.rev initBuffer)) (build_list_str initA)in
    let res = List.rev (theDivision initA initBuffer) in
    let () = Printf.printf "output is: %s (%s)\n" (build_list_str res) (string_of_int (to_int (0::res))) in
    res 
*)
      
let quot_n nA nB =
  if nA <<! nB then []
  else if equalN nA nB then [1]
  else 
    let b = List.rev nB and a = List.rev nA in
    let (initBuffer, initA) = getFirstsOfA (List.length b) a [] in
    let rec theDivision a buffer = 
      if buffer <<! nB then
        if a = [] then [0] else
          let (newBuffer, newA) = extBuff1 buffer a in 
          0::(theDivision newA newBuffer)
      else
        let newBuff = diff_n buffer nB in
        if a = [] then [1] else
          let (newBuffer, newA) = extBuff1 newBuff a in
          1::(theDivision newA newBuffer)
    in
    let (initBuffer, initA) = if initBuffer <<! nB then extBuff1 initBuffer initA else (initBuffer, initA) in
    List.rev (theDivision initA initBuffer)

let quot_b bA bB =
  match (bA, bB) with
    (_, []) -> failwith "division by zero" |
    ([], _) -> [] |
    (a::l1, b::l2) ->
      let naturalPart = quot_n l1 l2 in
      if naturalPart = [] then 
        []
      else
        (if a = b then 0 else 1)::(naturalPart)

(*
let quot_b bA bB =
  let rec div nA nB q =
    if nA >=! nB then
      div (diff_n nA nB) nB (add_n q [1])
    else q
  in
  match (bA, bB) with
    (_, []) -> failwith "division by zero" |
    ([], _::_) -> [] |
    (a::l1, b::l2) ->
      let (signA, signB) = (sign_b bA, sign_b bB) in
      let sign = (if signA*signB = 1 then 0 else 1) and q = (if l1 = l2 then [0;1] else div l1 l2 []) in
      if l1 = l2 then sign::[1] else
      if (signA < signB) then sign::(remove_zeros (add_n q [1])) else
        sign::(remove_zeros q)
*)


(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
*)
let mod_b bA bB =
  let q = quot_b bA bB in
  let m = mult_b bB q in
  let a = diff_b bA m in
  if a << [] then add_b a bB else a

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = (quot_b bA bB, mod_b bA bB)
