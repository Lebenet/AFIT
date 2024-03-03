(** Testing for primality *)

open Z
open Z_power

(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)
let is_prime n =
  if lt n (of_int 2) || (not (equal n (of_int 2))) && (equal (erem n (of_int 2)) zero) then
    false
  else
    let rec ri_p n x =
      if gt (mul x x) n then
        true
      else if equal (erem n x) zero then
        false
      else
        ri_p n (add x (of_int 2))
    in
    ri_p n (of_int 3)