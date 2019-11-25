(** Tweaking OCaml built-in euclidean division

The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)
let sign x = 0

(* Integer quotient implementation ; main use is in case of quotient
   of an integer by a natural number.
 *)

(** Quotient of an integer by a natural number.
    This is the quotient in euclidiant division sense.
    @param a dividend
    @param b natural number you divide by.
 *)
let quot a b = 0

(* Integer modulo implementations. Negative case need be taken into
   account ; representant is expected non-negative. This is not OCAML
   default.
 *)

(** Modulo of two integers.
    Following Euclidean division. NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    @param a input integer
    @param b moduli a natural number.
 *)
let modulo a b = 0

(* Integer modulo implementations. Negative case need be taken into
   account ; representant is expected non-negative. This is not OCAML
   default.
 *)

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
*)
let div a b = (0, 0)
