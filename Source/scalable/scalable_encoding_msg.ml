(** Encoding Strings *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
#mod_use "scalable_power.ml";;
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = String.length(str) - 1 and pwr = power [0;0;1] (from_int bits) in
  let rec get_chars i facteur = match i with
    |i when i < 0 -> [0;0]
    |i -> let lettre = from_int(Char.code(str.[i])) in
	  if lettre >>= pwr then invalid_arg("Error encoding string: bit format doesn't match the requirements for the given string.")
	  else add_b(get_chars (i-1) (mult_b facteur pwr)) (mult_b lettre facteur)
  in
  get_chars len [0;1];;
  
(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let pwr = power [0;0;1] (from_int bits) in
  let rec sub nbr = match nbr with
    |[0;0] -> ""
    |_ -> let lettre = mod_b nbr pwr in sub(quot_b nbr pwr) ^ Char.escaped(Char.chr(to_int lettre))
  in
  sub msg;;
