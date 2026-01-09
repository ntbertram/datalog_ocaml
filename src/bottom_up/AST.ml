
(* this file is part of datalog. See README for the license *)

(** {1 Parsing AST} *)
type comparison =
    | Geq
    | Leq
    | Eq
    | Neq
    | Lt
    | Gt


type file = clause list
  (** Toplevel statement *)
and clause =
  | Clause of literal * literal list
and literal =
  | Atom of string * term list
  | Comp of term * comparison * term
and term =
  | Var of string
  | Const of string
  | Quoted of string
and query =
  | Query of term list * literal list * literal list
  (** Query: projection, positive lits, negative lits *)


let neg_hack lit = 
    match lit with
    | Atom (s, t) -> Atom ("NEG_" ^ s, t)
    | _ -> failwith "Currently negation over comparisons is not supported"
