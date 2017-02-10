open Formulas

(* Converts the formula to CNF and simplifies it as much as possible. *)
val cnf : formula -> formula

(* Evaluates to true if and only if the formula is a tautology *)
val prove : formula -> bool
