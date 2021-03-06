(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (a,_) = key in
  let rec find i = match i with
    |_ when (a/i) * i = a -> (sign a * i, a/i)
    |1 -> invalid_arg("Couldn't find a prime pair matching this key, are you sure it's correct ?")
    |i -> find(i+1)
  in
  find (2) ;;
