(** Chinese remainder theorem *)

(*
#directory "../builtin";;
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
*)

open Builtin
open Basic_arithmetics


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
    @param l list of pairwise coprime factors of first argument.
    @param y list of remainders modulo pairwise relatively prime factors of m   
 *)
let rec crt_solver m l y =
  let test m l y = match (l, y) with
    |(p::q::[], k1::k2::[]) -> let (u,v,_) = bezout p q in
                               let a = (p*u*k2) + (v*q*k1)
                               in (p*q, a)
    |_ -> invalid_arg("Erreur: probleme inverse CRT par 2")
  in                                              
  let rec solver l y = match (l, y) with
    |(e1::s1::[], e2::s2::[]) -> test m l y
    |(e1::s1, e2::s2) -> let (a, b) = solver s1 s2 in test m (e1::[a]) (e2::[b])
	|(e1::[],e2::[]) -> e2
    |_ -> invalid_arg("Erreur: probleme inverse CRT par 1")
  in
  let (_,res) = solver l y in modulo res m;;
