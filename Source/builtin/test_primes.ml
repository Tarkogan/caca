(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = true

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
let is_pseudo_prime p test_seq = true
