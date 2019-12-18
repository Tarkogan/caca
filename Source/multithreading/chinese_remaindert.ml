(** Chinese remainder theorem *)

(*
#directory "../builtin";;
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
#mod_use "test_primes.ml";;
*)

open Builtin
open Basic_arithmetics
open Power
open Test_primes

(** Image of the Chinese Remainder map (list)
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)
let crt_image x l =
  let rec image list = match list with
    |[] -> []
    |e::s -> modulo x e :: image s
  in
  image l;;
  

(** Inverse image of Chinese Remainder map
    @para m a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)
let crt_solver m l y =
  let inverse_list y = match (y) with
    |e::s -> let (b,_,c) = bezout e m in if c = 1 then b::inverse s else inverse_list s
    |[] -> []
  in
  let inverse = inverse_list y in
  let rec image 
  
      
