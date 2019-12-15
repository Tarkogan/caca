(** Factoring bitarrays into primes *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
*)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (a,_) = key in
  let rec find i = match i with
    |_ when compare_b(mult_b (quot_b a i) i) a = 0 -> (mult_b i (from_int (sign_b a)),quot_b a i)
    |i when compare_b i [0;1] = 0 -> invalid_arg("Couldn't find a prime pair matching this key, are you sure it's correct ?")
    |i -> find(add_b i [0;1])		 
  in 
  find ([0;0;1]) ;;
