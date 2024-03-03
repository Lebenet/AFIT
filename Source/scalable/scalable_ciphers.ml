(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power



let modulo a b =
  let res = a mod b in
  if res < 0 then
    if b < 0 then
      res - b
    else
      res + b
  else
    res

let quot a b = (a - modulo a b) / b

let div a b = (quot a b, modulo a b)

let rec pow2 n = match n with
    1 -> true |
    n when modulo n 2 != 0 -> false |
    _ -> pow2 (n/2)


let bezout a b =
  let s = 0 and old_s = 1 and t = 1 and old_t = 0 and r = b and old_r = a in
  let rec rbezout s t r old_s old_t old_r = match r with
      0 -> (old_s, old_t, old_r) |
      _ -> let quotient = quot old_r r in
           rbezout (old_s - quotient*s) (old_t - quotient*t) (old_r - quotient*r) s t r
  in
  rbezout s t r old_s old_t old_r

let mod_powerN x n m =
  if n < 0 then invalid_arg "n negative" else
  let rec rmod_power x n = match n with
      1 -> modulo x m |
      n when modulo n 2 == 0 -> let p = rmod_power (modulo x m) (n / 2) in modulo (p*p) m |
      _ -> let p = rmod_power (modulo x m) (n / 2) in modulo (p*p*(modulo x m)) m
  in
  match n with
    0 -> 1 |
    n when pow2 n -> rmod_power x n |
    _ -> let rec exponentMod x n m =
             if x == 0 then
               0
             else if n == 0 then
               1
             else if modulo n 2 == 0 then
               let y = exponentMod x (n/2) m in
               let res = modulo (y * y) m in
               modulo (res + m) m
             else
               let y = modulo x m in
               let res = modulo (y * modulo (exponentMod x (n-1) m) m) m in
               modulo (res + m) m
         in
         exponentMod x n m


let prime_mod_powerN x n p =
  if n < 0 then invalid_arg "n negative" else
    if n == 0 then 1 else
      if x == 0 then 0 else
        let (q,r) = div n (p-1) in
        mod_powerN x r p

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)

let generate_keys_rsa p q = 
    if p = q then invalid_arg "equal p and q" else
      let n = mult_b p q and z = mult_b (diff_b p [0;1]) (diff_b q [0;1]) and e = (from_int 65537) in
      let (d,_,_) = bezout_b e z
      in ((n,e),(n,d));;

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = from_int (mod_powerN (to_int m) (to_int e) (to_int n))

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = from_int (mod_powerN (to_int m) (to_int d) (to_int n))
(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p =
  let rec rg g =
    if mod_b (power g [0;0;1]) p <> [0;1] then
      (g,p)
    else
      rg(add_b (from_int (Random.int 1000000)) [0;0;1])
  in 
    rg (add_b (from_int (Random.int 1000000)) [0;0;1]);;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let x = from_int (Random.int 1000000) in
  let priv = from_int (prime_mod_powerN (to_int g) (to_int x) (to_int p)) in
  (priv, x)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = Random.int (to_int p - 3) + 2 in
  (from_int (mod_powerN (to_int g) k (to_int p)), from_int (modulo ((to_int msg) * (mod_powerN (to_int kA) k (to_int p))) (to_int p)))

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let c1 = mod_powerN (to_int msgA) (to_int a) (to_int p) in
  let (invx, _, _) = bezout c1 (to_int p) in
  from_int (modulo ((to_int msgB) * modulo invx (to_int p)) (to_int p))
