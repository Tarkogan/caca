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
  let len = String.length(str) and pwr = power 2 bits in
  let rec get_chars i buffer = match i with
    |i when i = len -> buffer
    |i -> let lettre = Char.code(str.[i]) in
	  if lettre > pwr then invalid_arg("Error encoding string: bit format doesn't match the requirements of the given string.")
	  else get_chars (i+1) (buffer * 1000 + lettre)
  in
  get_chars  0 0;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits = ""



  (*V encode "Bashar" 7 -> 2294023860466*)
  (*X encode "Bashar" 7 -> 2299610319532*)
  
(*
114
97
104
115
97
66
*)
  
