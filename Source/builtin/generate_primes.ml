(** Generating primes *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
*)

open Builtin
open Basic_arithmetics

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)
let init_eratosthenes n =
  if n < 2 then invalid_arg("Error init_eratosthenes: number of values must be at least 2.")
  else
  let rec truc i = match i with
    |x when x > n -> []
    |x -> x::truc(i+2)
  in
  2::truc 3;;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*) (*Refaire en suivant la méthode du crible*)
let eratosthenes n =
  if n < 2 then invalid_arg("Error eratosthenes: number of values must be at least 2.")
  else
    let liste = init_eratosthenes in
    let rec divide nbr i = match modulo nbr i with
      |_ when i*i > abs(nbr) -> true
      |0 -> false
      |_ -> divide nbr (i+2)
    in
    let rec test i = match i with
      |x when x > n -> []
      |x -> if divide i 3  then i::test(i+2) else test(i+2)
    in
    2::test 3;;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let file = open_out file in
  let rec write_in liste fichier = match liste with
    |[] -> close_out file
    |e::s -> Printf.fprintf fichier "%d\n" e; write_in s fichier
  in  
  write_in li file;;
  

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
  let liste = eratosthenes n in write_list liste file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let try_read() = input_line_opt in_c
  in
  let rec listing() = match try_read() with
    |Some s -> int_of_string(s)::(listing())
    |None -> close_in in_c;[]
  in
  listing();;
      
(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = let file = open_in file in create_list file;;

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: Your list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime numbers for specific or fun
   purposes.
*)

(**
   creates a list of couples of prime numbers linked to eachother by the relation f
*)
let prime_couples limit isprime f =
  let prime_list = eratosthenes limit in
  let rec couple liste = match liste with
    |[] -> []
    |e::s -> let nbr = f(e) in if not(isprime(nbr)) then couple s else (e,nbr)::couple s
  in
  couple prime_list;;

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime = prime_couples limit isprime (function x -> 2*x+1);;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = (2,3)::prime_couples limit isprime (function x -> x+2);;
