(** Basic arithmetics for ordered euclidian ring. *)

(*
#use "scalable.ml";;
*)

open Scalable

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
  
(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB =
  if sign_b(bA) = 0 || sign_b(bB) = 0 then invalid_arg "a and b must be non zero integers"
  else
    let rec pgcd max = match mod_b (abs_b(bA)) max with 
      |x when compare_b x [0;0] = 0 && compare_b (mod_b (abs_b(bB)) max) [0;0] = 0 -> abs_b max
      |_ when compare_b (diff_b max [0;1]) [0;0]  = 0 -> from_int 1	
      |_ -> pgcd (add_b max (from_int(-1)))
    in
    clear_b(pgcd (if abs_b(bA) >> abs_b(bB) then abs_b(bB) else abs_b(bA))@[0;0]);;
	

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  if sign_b(bA) = 0 || sign_b(bB) = 0 then invalid_arg "a and b must be non zero integers"
  else
    let rec bez(u1,v1,u2,v2,r1,r2) = match r2 with
      |x when compare_b (clear_b x) [0;0]  = 0 -> (clear_b u1, clear_b v1,clear_b r1)
      |_ -> let q = quot_b r1 r2 in
	    let (u3,v3,r3) = (diff_b u1 (mult_b q u2), diff_b v1 (mult_b q v2), diff_b r1 (mult_b q r2))
	    in bez(u2,v2,u3,v3,r2,r3)
    in
    bez([0;1],[0;0],[0;0],[0;1],bA,bB);;
