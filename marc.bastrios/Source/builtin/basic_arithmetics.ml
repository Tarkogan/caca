(** Basic arithmetics with built-in *)

(*
#use "builtin.ml";;
*)
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
  if a = 0 || b = 0 then invalid_arg("param 'a' and 'b' non-zero integers")
  else 
  let rec bez(u1,v1,u2,v2,r1,r2) =  (* ul-2,vl-2,rl-2 | ul-1,vl-1,rl-1 *)
    match r2 with
      |x when x = 0 -> (u1,v1,r1)
      |_ -> let q = r1/r2 in let (u3,v3,r3) = (u1-q*u2, v1-q*v2, r1-q*r2) in bez(u2,v2,u3,v3,r2,r3)
  in
  bez(1,0,0,1,a,b);; (*u0,v0 = 1,0 et u1,v1 = 0,1*)
