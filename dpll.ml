(* Naive, slow implementation of the Davis-Putnam-Logeman-Loveland (DPLL) method for *)
(* proving statements in Propositional Logic.                                        *)

open Formulas

(* Extracts the name of the identifier from a literal *)
let identifier_of literal =
    match literal with
    | Literal(a)        -> a
    | NegatedLiteral(a) -> a

(* A predicate which indicates whether a clause is a tautology or not *)
let rec tautological (clause: clause) =
    match clause with
    | []                      -> false
    | (Literal(l)::ls)        -> (List.mem (NegatedLiteral(l)) ls) || tautological ls
    | (NegatedLiteral(l)::ls) -> (List.mem (Literal(l)) ls)        || tautological ls

(* Propagates the given literal over the set of clauses *)
let propagate_literal literal (clauses: clause list) =
    match literal with
    | NegatedLiteral(_) -> clauses
    | Literal(l) ->

        let rec propagate_clause clause accumulator =
            match clause with
            | []                        -> Some(accumulator)
            | (NegatedLiteral(l')::ls') -> propagate_clause ls' (if l' = l then accumulator else (NegatedLiteral(l') :: accumulator))
            | (Literal(l')::ls')        -> if l' = l then None else (propagate_clause ls' (Literal(l') :: accumulator))
        in
            ListExt.filter_map (fun clause -> propagate_clause clause []) clauses

(* Propagates the given set of literals over the given set of clauses *)
let rec propagate (literals: literal list) (clauses: clause list) =
    match literals with
    | [] -> clauses
    | (literal::ls) -> propagate ls (propagate_literal literal clauses)

(* Predicate which indicates whether the provided literal is pure through *)
(* in the given set of clauses                                            *)
let rec is_pure literal (clauses: clause list) =
    match (literal, clauses) with
    | (Literal(_), [])           -> true
    | (Literal(l), (clause::cs)) -> (not (List.mem (NegatedLiteral(l)) clause)) && is_pure literal cs
    | (NegatedLiteral(_), _)     -> false

(* Evaluates to some identifier if the clause provided is unit; otherwise none *)
let rec literal_if_unit (clause: clause) =
    match clause with
    | [Literal(l)]        -> Some(Literal(l))
    | [NegatedLiteral(l)] -> Some(NegatedLiteral(l))
    | _                   -> None

(* Choose an arbitrary literal identifier from within the set of clauses. *)
let choose_identifier clauses = identifier_of (List.hd (List.hd clauses))

(* Performs the DPLL algorithm on the provided clauses. Returns true if the clauses can be *)
(* satisfied, and false if not.                                                            *)
let rec dpll clauses =
    (* Remove tautological clauses *)
    let clauses' = (List.filter (fun clause -> not(tautological clause)) clauses) in

    (* Propagate all unit clauses *)
    let unit_literals = ListExt.filter_map (fun clause -> literal_if_unit clause) clauses' in
    let clauses''     = propagate unit_literals clauses' in

    (* If we have deleted all remaining clauses then we can satisfy these clauses *)
    let satisfiable = (clauses'' = []) in

    (* If we have an empty clause we have found a contradiction *)
    let contradiction = (List.mem [] clauses'') in

    if satisfiable || contradiction then
        satisfiable && not contradiction
    else
        let split          = choose_identifier clauses'' in
        let positive_split = propagate_literal (Literal(split)) clauses'' in
        let negative_split = propagate_literal (NegatedLiteral(split)) clauses'' in

        dpll positive_split || dpll negative_split;;