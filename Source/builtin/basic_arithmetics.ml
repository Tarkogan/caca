(** Basic arithmetics with built-in *)

(*#use "builtin.ml";;*)
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
let bezout a b = (0,0,0) (*
  let (res1, res2) = (1,1) in
  let rec euclide(a, b, goal) =
    match modulo a b with
    |x when x = goal -> (a, - quot a b, a)
    |x -> euclide(b, modulo a b, goal)
  in
  let rec bez(u,v,a_temp,b_temp)=
    if a_temp = a then res1 = res1 + u else let (x,q,y) = euclide(a,b,a_temp) in bez ()

  in

  euclide(710, 310, 710, 40);;*)
