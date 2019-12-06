(** Power function implementations for built-in integers *)

open Builtin
open Basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  if n < 0 then 0
  else
    let rec powa truc i =
      match i with
	|y when y = n -> truc
	|_ -> powa (truc * x) (i+1)
    in
    (powa 1 0);;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  let rec powa b =
    let parity = if b mod 2 = 0 then 1 else x in
    match b with
      |0 -> 1
      |1 -> x
      |_ -> let powa = powa(b/2) in parity * powa * powa
  in
  powa(n);;

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m = modulo (power x n) m;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  let facteur = if n > p then (n-p+1) else n
  in
  modulo (power x facteur) p;;
