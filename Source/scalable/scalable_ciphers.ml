
(** Ciphers
    bitarrays based ciphers.
*)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
#mod_use "scalable_power.ml";;
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q =
  if compare_b p q = 0 then invalid_arg("Error generate_keys_rsa: both arguments have to be distinct prime numbers.")
  else
    let n = (mult_b p q)
    and phi = mult_b (diff_b p [0;1]) (diff_b q [0;1]) in
    let rec dice d e = match d with
      |_ when compare(mod_b(mult_b d e) phi) [0;1] = 0 -> (e,d)
      |[0;1] -> invalid_arg("error generating rsa key: invalid parameters")
      |_ -> dice (diff_b d  [0;1]) e
    in 
    let rec key i = match i with
      |x when let (_,_,c) = bezout_b i phi in compare_b c [0;1] = 0 -> dice (diff_b phi [0;1]) i
      |[0;1] -> invalid_arg("error generating rsa key: invalid parameters")
      |_ -> key (diff_b i [0;1])
    in
    let (e,d) = key (diff_b phi [0;1]) in ((n,e),(n,d));;

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p = (p, quot_b(diff_b p [0;1]) [0;0;1]);;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let a = add_b( (from_int (Random.int(10000)))) (diff_b g [0;0;1])  in
  (prime_mod_power g a p, a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = quot_b (mult_b p (from_int 7)) (from_int 11) in
  (prime_mod_power g k p, mult_b msg (prime_mod_power kA k p));;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  mod_b (quot_b msgB (prime_mod_power msgA a p)) p;;



