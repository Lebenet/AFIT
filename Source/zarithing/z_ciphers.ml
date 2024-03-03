(** Ciphers
    Big integers based ciphers.
*)

open Z
open Z_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  if equal p q then invalid_arg "p equals q"
  else
    let n = mul p q and z = mul (sub p one) (sub q one) in
    let e = sub (ediv z (succ one)) one in
    let d = invert e z in
    ((n,e),(n,d))

(** Encryption using RSA cryptosystem.
    @param m big integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n

(** Decryption using RSA cryptosystem.
    @param m big integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
(* auxiliary methods to help me find the first primitive root of p *)
let rec remove_duplicates li =
  let rec remove_dupli li x = match li with
      [] -> [] |
      e::l -> if e = x then remove_dupli l x else e::(remove_dupli l x)
  in match li with
       [] -> [] |
       e::l -> e::(remove_duplicates (remove_dupli l e))

let getMult x =
  let rec aux y i =
    if leq y one then []
    else
    if equal (erem y i) zero then
      i::(aux (ediv y i) i)
    else
      aux y (add i one)
  in
  aux x (succ one)

let rec getPowers x l = match l with
    [] -> [] |
    e::l -> (ediv x e)::(getPowers x l)

let getFirstPrimitiveRoot p powers =
  let rec testPowers i =
    let rec isFine powers = match powers with
        [] -> true |
        n::powers -> not (equal (mod_power i n p) one) && isFine powers
    in
    if equal i p then minus_one
    else
      if isFine powers then
        i
      else
        testPowers (add i one)
  in testPowers (succ one)


let rec public_data_g p =
  let s = sub p one in
  let firstPrimRoot = getFirstPrimitiveRoot p (getPowers s (remove_duplicates (getMult s))) in
  ((mod_power firstPrimRoot (ediv s (succ one)) p), p)

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = let x = add (random_int (sub p (succ one))) one in (mod_power g x p, x)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = add (random_int (sub p (of_int 3))) (succ one) in
  (mod_power g k p, erem (mul msg (mod_power kA k p)) p)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let c1 = mod_power msgA a p in
  let invx = invert c1 p in
  erem (mul msgB (erem invx p)) p
