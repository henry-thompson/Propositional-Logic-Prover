open Formulas

(* Simplifies the formula bottom up, converting to CNF and using known identities. *)
let rec cnf x =
    let cnf' x' =
        match x' with
        (* Remove implications. *)
        | Implication(p, q) -> cnf (Or(Not p, q))
        | Iff(p, q)         -> cnf (And(Or(Not p, q), Or(Not q, p)))

        (* Push in negations. *)
        | Not(Not(p))       -> p
        | Not(And(p, q))    -> cnf (Or(Not p, Not q))
        | Not(Or(p, q))     -> cnf (And(Not p, Not q))
        
        (* Simplify conjunctions. *)
        | And(True, True)   -> True
        | And(True, q)      -> q
        | And(p, True)      -> p
        | And(False, q)     -> False
        | And(p, False)     -> False
        | And(p, Not(q))    -> if p = q then False else x'
        | And(Not(p), q)    -> if p = q then False else x'
        | And(p, q)         -> if p = q then p else x'
        
        (* Simplify disjunctions. *)
        | Or(True, q)       -> True
        | Or(p, True)       -> True
        | Or(False, False)  -> False
        | Or(False, q)      -> q
        | Or(p, False)      -> p
        | Or(p, Not(q))     -> if p = q then True else x'
        | Or(Not(p), q)     -> if p = q then True else x'

        (* Push disjunctions in until we only have them apply to literals to obtain CNF form. *)
        | Or(And(p, q), r)  -> cnf (And(Or(p, r), Or(q, r)))
        | Or(p, And(q, r))  -> cnf (And(Or(p, q), Or(p, r)))

        | Or(p, q)          -> if p = q then p else x'

        | _                 -> x'
    
    in mapf cnf' x;;

(* true if the formula x is a tautology; else evaluates to false *)
let prove x = (cnf x) = True;;
