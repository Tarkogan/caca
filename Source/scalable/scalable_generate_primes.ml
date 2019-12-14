(** Generating prime bitarrays *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
#mod_use "scalable_power.ml";;
#mod_use "scalable_test_primes.ml";;
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_test_primes

(* Initializing list of bitarrays for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  if n < 2 then invalid_arg("Error init_eratosthenes: number of values must be at least 2.")
  else
    let n = from_int n in
    let rec truc i = match i with
      |x when x >> n -> []
      |x -> x::truc(add_b i [0;0;1])
    in
    [0;0;1]::truc([0;1;1]);;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  if n < 2 then invalid_arg("Error init_eratosthenes: number of values must be at least 2.")
  else
    let n = from_int n in
    let rec test i = match i with
      |x when x >> n -> []
      |x -> if is_prime x then x::test(add_b i [0;0;1]) else test(add_b i [0;0;1])
    in
    [0;0;1]::test [0;1;1];;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file =
  let file = open_out file in
  let rec listing list = match list with
    |[] -> ""
    |[e] -> string_of_int(e) (*évite la virgule en fin de ligne*)
    |e::s -> string_of_int(e) ^ "," ^ listing s
  in
  let rec write_in liste = match liste with
    |[] -> close_out file
    |e::s -> Printf.fprintf file "%s\n" (listing e); write_in s
  in
  write_in li;;
 

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
  let list = eratosthenes n in write_list list file;;
(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c =
  let try_read() = input_line_opt in_c
  in
  let rec sorting i ligne = match i with
  |x when x = String.length(ligne) -> []
  |_ -> if ligne.[i] != ',' then int_of_string(Char.escaped(ligne.[i]))::sorting (i+1) ligne else sorting (i+1) ligne
  in 
  let rec listing() = match try_read() with
    |Some s -> sorting 0 s::listing()
    |None -> close_in in_c; []
  in
  listing();;

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let file = open_in file in create_list file;;

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime bitarrays for specific or fun
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

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
  prime_couples limit isprime (function x -> add_b(mult_b  x  [0;0;1]) [0;1]);;
   

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  prime_couples limit isprime (function x -> add_b x  [0;0;1]);;
