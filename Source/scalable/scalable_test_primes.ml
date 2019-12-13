(** Testing for primality *)

(*
#mod_use "scalable.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n =
  let rec sub i = match mod_b (abs_b n) i with
    |_ when mult_b i i >> abs_b n -> true
    |[0;0] -> false
    |_ -> sub(add_b i [0;0;1])
  in
  match abs_b n with
    |[0;0]|[0;1] -> false
    |[0;0;1] -> true
    |_ -> compare_b(mod_b (abs_b n) [0;0;1]) [0;0] != 0 && sub [0;1;1];; 

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays against which to test
 *)
let is_pseudo_prime p test_seq =
  let rec fermat liste = match liste with
    |[] -> true
    |e::s -> let e = mod_b e p in
	     if compare_b e [0;0] = 0 then fermat s
	     else if mod_power e (diff_b p [0;1]) p = [0;1] then fermat s
	     else false
  in
  fermat test_seq;;
