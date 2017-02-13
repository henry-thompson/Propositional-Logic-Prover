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

(* A literal is either of form A or ¬A *)
type literal =
   | Literal        of identifier
   | NegatedLiteral of identifier

(* A clause is a disjunction over literals: *)
(* ¬K_1, ..., ¬K_m, L_1, ..., ¬L_n          *)
type clause = literal list

(* Helper function for mapping over every term in a formula *)
let rec mapf f x =
    match x with
    | Atom(t)           -> f (Atom t)
    | And(p, q)         -> f (And(mapf f p, mapf f q))
    | Or(p, q)          -> f (Or(mapf f p, mapf f q))
    | Not(p)            -> f (Not(mapf f p))
    | Implication(p, q) -> f (Implication(mapf f p, mapf f q))
    | Iff(p, q)         -> f (Iff(mapf f p, mapf f q))
    | True              -> f True
    | False             -> f False;;