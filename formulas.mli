(* The name of an atomic symbol in propositional logic *)
type identifier = string

(* A propositional logic formula *)
type formula =
   | Atom        of identifier
   | And         of formula * formula
   | Or          of formula * formula
   | Not         of formula
   | Implication of formula * formula
   | Iff         of formula * formula
   | True
   | False

(* Helper function for mapping over every term in a formula. *)
val mapf : (formula -> formula) -> formula -> formula