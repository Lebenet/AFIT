(** Ciphers
    Builtin integer based ciphers.
*)
open Builtin
open Basic_arithmetics
open Power

(*
#use "builtin.ml";;
#use "basic_arithmetics.ml";;
#use "power.ml";;
#use "generate_primes.ml";;
*)

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)

let rec cesar k sign b = function (*encrypt: sign = 1, decrypt: sign = -1*)
    [] -> [] |
    e::l -> ((e + sign*k) mod b)::(cesar k sign b l)

let encrypt_cesar k m b = cesar k 1 b m

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b = cesar k (-1) b m


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
 *)


let generate_keys_rsa p q =
  if p = q then invalid_arg "p equals q"
  else
    let n = p*q and z = (p-1)*(q-1) in
    let e = z/2-1 in
    let d = let (d2, _, _) = (bezout e z) in modulo d2 z in
    ((n,e),(n,d))

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
(* auxiliary methods to help me *)




let rec remove_duplicates li =
  let rec remove_dupli li x = match li with
      [] -> [] |
      e::l -> if e = x then remove_dupli l x else e::(remove_dupli l x)
  in match li with
       [] -> [] |
       e::l -> e::(remove_duplicates (remove_dupli l e))

let getMult x =
  let rec aux y i =
    if y <= 1 then []
    else
    if y mod i = 0 then
      i::(aux (y / i) i)
    else
      aux y (i+1)
  in
  aux x 2

let rec getPowers x l = match l with
    [] -> [] |
    e::l -> (x/e)::(getPowers x l)

let getFirstPrimitiveRoot p powers =
  let rec testPowers i =
    let rec isFine powers = match powers with
        [] -> true |
        n::powers -> mod_power i n p != 1 && isFine powers
    in
    if i = p then (-1)
    else
      if isFine powers then
        i
      else
        testPowers (i+1)
  in testPowers 2

let getXElGamal p = Random.int (p-2) +1

let getKElGamal p = Random.int (p-3) +2

let rec public_data_g p =
  let s = p - 1 in
  let firstPrimRoot = getFirstPrimitiveRoot p (getPowers s (remove_duplicates (getMult s))) in
  ((mod_power firstPrimRoot (s/2) p), p)

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = let x = getXElGamal p in (mod_power g x p, x)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = getKElGamal p in
  (mod_power g k p, modulo (msg * (mod_power kA k p)) p)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let c1 = mod_power msgA a p in
  let (invx, _, _) = bezout c1 p in
  modulo (msgB * modulo invx p) p
