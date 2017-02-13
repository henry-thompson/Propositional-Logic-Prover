(* Performs the DPLL algorithm on the provided clauses. Returns true if the clauses can be *)
(* satisfied, and false if not.                                                            *)
val dpll : (clause list) -> bool