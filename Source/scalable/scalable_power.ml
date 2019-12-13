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
  if sign_b n = -1 then invalid_arg("power: exponent must be positive")
  else 
  let rec powa b =
    let parity = if compare_b(mod_b b (from_int 2)) [0;0] = 0 then from_int 1 else x
    in match b with
      |[0;0] -> [0;1]
      |[0;1] -> x
      |_ -> let powa = (powa (quot_b b [0;0;1])) in mult_b parity (mult_b powa powa)
  in
  powa n;;
    

(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
  if sign_b n = -1 || sign_b m = -1 then invalid_arg("mod_power: exponent/base must be positive")
  else
    let rec powa base exp n =
      let base = mod_b base m in match mod_b exp [0;0;1] with
	|_ when compare_b exp [0;0] = 0 -> [0;1]
	|_ when compare_b exp [0;1] = 0 -> base
	|x when compare_b x [0;0] = 0 -> powa (mod_b (mult_b base base) n) (quot_b exp [0;0;1]) n
	|_ -> mod_b (mult_b base (powa base (diff_b exp [0;1]) n)) m
    in
    if compare_b x [0;0] = 0 then [0;0] else powa x n m;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  if sign_b n = -1 || sign_b p = -1 then invalid_arg("mod_power: exponent/base must be positive")
  else
    let facteur = mod_b n (diff_b p [0;1])
    in match (n <= [0;0]) with
      |true when compare_b n [0;0] = 0 -> [0;1]
      |true -> [0;0]
      |false -> mod_power x facteur p;; 
