(** Basic arithmetics with built-in integers *)

open Builtin

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)
let rec gcd a b =
  if a = 0 || b = 0 then invalid_arg "a and b must be non zero integers"
  else
    let rec pgcd max = match a mod max with
      |_ when max = 0 -> 1
      |0 when b mod max = 0 -> abs(max)
      |_ -> pgcd (max - 1)
    in
    pgcd(if abs(a) > abs(b) then abs(b) else abs(a));;



(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let pgcd = gcd a b in
  (quot a pgcd, quot b pgcd, pgcd);;
