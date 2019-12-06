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
let bezout a b =
  let (u, v) = (0,0) in
  let rec euclide(a_t, b_t, goal) =
    match modulo a_t b_t with
    |x when x = goal -> (a, - quot a b, a)
    |x -> euclide(b, modulo a b, goal) (*-> (a,q,b,r) with a = qb + r*)
  in

  let rec bez(obj, facteur) =
    let (a_temp,q,b_temp) = euclide(a,b,obj) in
    match (a_temp,b_temp) with
      |(x,y) when (x,y) = (a,b) -> (facteur * x, facteur * y)

      |(x,y) when x = a -> let (a_prime,b_prime) = bez(y,facteur*q) in (facteur*x+a_prime,b_prime) (* a - qy = r*)
      |(x,y) when x = b -> let (a_prime,b_prime) = bez(y,facteur*q) in (0+a_prime,facteur*x+b_prime) (* b - qy = r*)

      |(x,y) -> let (xa_prime,xb_prime) = bez(x,facteur) and (ya_prime,yb_prime) = bez(y,facteur) in (xa_prime + ya_prime, xb_prime + yb_prime)
  in 

  bez(gcd a b, 1);;



  (* *)
