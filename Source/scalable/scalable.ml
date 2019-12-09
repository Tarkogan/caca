(** A naive implementation of big integers

    This module aims at creating a set of big integers naively. Such data
    types will be subsequently called bitarrays. A bitarray is a list of
    zeros and ones ; first integer representing the sign bit. In this
    context zero is reprensented by the empty list []. The list is to
    be read from left to right ; this is the opposite convention to the
    one you usually write binary decompositions with. After the sign bit
    the first encountered bit is the coefficient in front of two to
    the power zero. This convention has been chosen to ease writing
    down code. A natural bitarray is understood as being a bitarray of
    which you've taken out the sign bit, it is just the binary
    decomposition of a non-negative integer.

*)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  let rec power i pow = match pow>abs(x) with
    |true -> pow/2 
    |false-> power (i+1)(pow*2) in
  let power = power 1 2
  in
  let rec binary nbr pwr = match pwr with
    |0 -> []
    |x -> binary (nbr mod pwr ) (pwr/2)@[nbr / pwr]
  in
  let rec complement invert liste = match liste with
    |[] -> []
    |e::s when invert = 1 -> (if e = 1 then 0 else 1)::complement 1 s
    |e::s when e = 1 -> e::complement 1 s
    |e::s -> e::complement 0 s
  in 
  match x with
    |x when x<0 -> 1::complement 0 (binary (abs(x)) power)
    |x -> 0::binary (abs(x)) power;;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
*)
let to_int bA =
  let rec get_number power liste inverted_nipples = match liste with
    |[] -> 0
    |e::s when inverted_nipples = 1 && e = 1 -> power + get_number (power*2) s 2
    |e::s when inverted_nipples = 1 && e = 0 -> get_number (power*2) s 1
    |e::s when inverted_nipples = 2 -> let truc = if e = 1 then 0 else power in truc + get_number (power*2) s 2  
    |e::s when e > 1 || e < 0 -> invalid_arg("to_int: list's format is invalid.")
    |e::s -> power * e + get_number (power * 2) s 0
  in
  match bA with
    |[] -> 0
    |1::s -> let result = (get_number 1 s 1)  in -result
    |_::s -> let result = (get_number 1 s 0)  in result;;

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
*)
let print_b bA =
  let rec get_string liste = match liste with
    |[] -> ""
    |e::s -> get_string s ^ string_of_int(e)
  in
  match bA with
    |debut::bA -> print_string(string_of_int(debut) ^ get_string bA)
    |_ -> print_string("");;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(*install_printer print_b;;*)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
    Assumed non-negative.
    @param nB A natural.
*)
let rec compare_n nA nB =
  let (a,b) = (to_int(0::nA),to_int(0::nB)) in
  match a >= b with
    |true when a = b -> 0
    |true -> 1
    |false -> -1;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (>>!) nA nB = compare_n nA nB = 1;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<<!) nA nB = compare_n nA nB = (-1);;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (>=!) nA nB = compare_n nA nB = 1 || compare_n nA nB = 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<=!) nA nB = compare_n nA nB = (-1) || compare_n nA nB = 0;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
  let (a,b) = (to_int(bA),to_int(bB)) in
  match a >= b with
    |true when a = b -> 0
    |true -> 1
    |false -> -1;; 

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<<) bA bB = compare_b bA bB = 1;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (>>) bA bB = compare_b bA bB = (-1);;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<<=) bA bB = compare_n bA bB = 1 || compare_n bA bB = 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (>>=) bA bB = compare_n bA bB = (-1) || compare_n bA bB = 0;;
;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
  |[]-> invalid_arg("sign_b: given list appears to be empty.")
  |[x] -> invalid_arg("sign_b: given list doesn't meet the requirements.")
  |e::s when e>1 || e<0 -> invalid_arg("sign_b: given list doesn't meet the requirements.")
  |e::s -> if e=0 then 1 else -1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = from_int(sign_b(bA)*to_int(bA));;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a =
  if a >= 4 then invalid_arg("_quot_t: parameter must be an integer smaller than 4")
  else if a < 0 && a mod 2 != 0 then a / 2 - 1 else a / 2;;


(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a =
  if a >= 4 then invalid_arg("_quot_t: parameter must be an integer smaller than 4")
  else a - 2*_quot_t a;;

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a);;

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB = from_int(to_int(0::nA) + to_int(0::nB));;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let (a,b) = (to_int(0::nA),to_int(0::nB)) in
  if a < b then invalid_arg("diff_n: first natural must be larger than the second")
  else from_int(a - b);;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let add_b bA bB = from_int(to_int(bA) + to_int(bB));;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = from_int(to_int(bA) - to_int(bB));;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d =
  if d < 0 then invalid_arg("shift: second argument must be positive")
  else 
    let truc = if d > List.length(bA) then List.length(bA) else d in
    let rec create i = match i with
      |0 -> []
      |i -> 0::create (i-1)
    in
    let rec delete i liste = match (liste,i) with
      |(_,0) -> liste
      |([],_) -> []
      |(e::s,i) -> delete (i-1) s
    in
    delete d bA@create truc;;


(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = from_int(to_int bA * to_int bB);;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =
  let (a,b) = to_int bA, to_int bB in
  if b = 0 then invalid_arg("quot_b: second argument must be a non zero Bitarray")
  else
    let res = if a < 0 && a mod b != 0 then a / b - sign_b bB else a / b in from_int(res);;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
*)
let mod_b bA bB =
  let (a,b,quot) = (to_int bA,to_int bB,to_int(quot_b bA bB)) in
  from_int(a - b*quot);;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = (quot_b bA bB, mod_b bA bB);;


