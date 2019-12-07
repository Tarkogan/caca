(** Ciphers
    Built-in integer based ciphers.
*)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
 *)
let encrypt_cesar k m b =
  let rec change liste = match liste with
    |[] -> []
    |e::s -> modulo(e+k) b ::change s
  in
  change m;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
 *)
let decrypt_cesar k m b =
  let rec change liste = match liste with
    |[] -> []
    |e::s -> modulo(e-k) b ::change s
  in
  change m;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  if p = q then invalid_arg("Error generate_keys_rsa: both arguments have to be distinct prime numbers.")
    else
    let (n,o) = (p*q,(p-1)*(q-1)) in
    let rec dice d e = match d with
      |x when modulo (e*x) o = 1 -> (e,d)
      |1 -> invalid_arg("error generating rsa key: invalid parameters")
      |x -> dice (x-1) e
    in
    let rec key i = match i with
      |x when let (_,_,c) = (bezout o x) in c = 1 -> dice (o-1) i
      |1 -> invalid_arg("error generating rsa key: invalid parameters")
      |x -> key (i-1)
    in
     
    let (e,d) = key (o-1) in ((n,e),(n,d));;
    


(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = (0, 0)

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = (0, 0)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = (0, 0)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = 0
