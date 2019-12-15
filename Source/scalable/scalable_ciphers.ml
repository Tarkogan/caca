
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
    and o = mult_b (diff_b p [0;1]) (diff_b q [0;1]) in
    
    let rec dice d e = print_string("dice\n");match d with
      |x when compare_b(mod_b (mult_b e x) o) [0;1] = 0 -> (e,d)
      |[0;1] -> invalid_arg("error generating rsa key: invalid parameters")
      |x -> dice (diff_b x [0;1]) e
    in
    let rec key i k = match i with
      |x when let (_,_,c) = (bezout_b o x) in compare c [0;1] = 0 -> dice (diff_b o [0;1]) i
      |[0;1] -> invalid_arg("error generating rsa key: invalid parameters")
      |x -> print_int(k); key (diff_b i [0;1]) (k+1)
    in
    
    let (e,d) = key (diff_b o [0;1]) 0 in ((n,e),(n,d));;
    

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = []

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = []

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p = ([], [])

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = ([], [])

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = ([], [])

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = []
