(** Testing for primality *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
*)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n =
  let rec sub i = match modulo (abs(n)) i with
    |_ when i*i > abs(n) -> true
    |0 -> false
    |_ -> sub(i+2)
  in
  match abs(n) with
    |0|1 -> false
    |2 -> true
    |_ -> modulo (abs(n)) 2 != 0 && sub 3;; 
  
  

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec fermat liste = match liste with
    |[] -> true
    |e::s -> let e = modulo e p in if e = 0 then fermat s else if mod_power e (p-1) p = 1 then fermat s else false
  in
  fermat test_seq;;

  
