(** Encoding Strings *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
*)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = String.length(str)-1 and pwr = power 2 bits in
  let rec get_chars i facteur  = match i with
    |i when i < 0 -> 0
    |i -> let lettre = Char.code(str.[i]) in
	  if lettre >= pwr then invalid_arg("Error encoding string: bit format doesn't match the requirements for the given string.")
	  else get_chars (i-1)(facteur * pwr) + lettre*facteur
  in
  get_chars (len) 1;;
  

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let pwr = power 2 bits in
  let rec sub nbr= match nbr with
    |0 -> ""
    |_ -> let lettre = modulo nbr pwr in sub (nbr/pwr) ^ Char.escaped(Char.chr(lettre))
  in
  sub msg;;
  
