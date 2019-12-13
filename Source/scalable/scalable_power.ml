(** Power function implementations for bitarrays *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
*)

open Scalable
open Scalable_basic_arithmetics

let rec clear_b bA =
  let rec last = function
    |[e] -> e
    |e::s -> last s
    |_ -> failwith("clear_l: could not verify last character.")
  in
  let rec delete = function
    |[e] -> []
    |e::s -> e::delete s
    |_ -> failwith("clear_l: could not delete last character.")
  in
  match bA with
    |[_;_] -> bA
    |[_]|[] -> invalid_arg("clear_l: given list appears to be empty.")
    |_ -> if last bA = 0 then clear_b (delete bA) else bA;;

(* Naive and fast exponentiation ; already implemented in-class in the
   built-in integer case.
*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
*)

let pow x n =
  if sign_b n = -1 then invalid_arg("pow: exponent must be a non negative Bitarray")
  else
    let rec powa truc i = match i with
      |y when compare_b y (clear_b n) = 0 -> truc
      |_ -> powa (mult_b truc x) (add_b i [0;1])
    in
    powa [0;1] [0;0];;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  let rec powa b =
    let parity = if (mod_b b [0;0;1]) << [0;0]  then [0;1] else x in
    match b with
      |x when compare_b x [0;0] = 0 -> [0;1]
      |x when compare_b x [0;1] = 0 -> x
      |_ -> let powa = powa(quot_b b [0;0;1]) in mult_b parity (mult_b powa powa)
  in
  powa(n);;

(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m = []

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p = []
