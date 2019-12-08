(** Factoring Built-In Int Primes *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
*)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (a,_) = key in
  let rec find i = match i with
    |_ when (a/i) * i = a -> (a/i, sign a * i)
    |1 -> invalid_arg("Couldn't find a prime pair matching this key, are you sure it's correct ?")
    |i -> find(i-1)
  in
  find (abs(a)-1);;
