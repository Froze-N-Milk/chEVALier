(*
(** types *)
type ty =
  (* currently used to label types that must be inferred *)
  | Unknown
  (* a type variable *)
  | Var of int
  | Unbound of string
  | Invalid
  | Never
  | Unit
  | Bool
  | Int
  | Real
  | String
  | Product
  | Function
  | Constructor of int
  | Construction of ty * ty list


  *)

(** expression-embedded values *)
type value =
  (* debruijn indexed variable *)
  | Var of int
  | Unbound of string
  | Unit
  | Bool of bool
  | Int of int
  | Real of float
  | String of string
  | Procedure of proc

and proc =
  (* arbitrary expression *)
  | Expr of expr
  (* builtin procedures *)
  (* halt *)
  (* halts with exit code {0} *)
  | Halt
  (* records *)
  (* constructs a record from the arguments {n}..{1}, continuing with {0} *)
  | Cons
  (* accesses the {2}th field of the record {1}, continuing with {0} *)
  | Proj
  (* boolean operations *)
  (* returns the boolean negation of {1}, continuing with {0} *)
  | Not
  (* returns the boolean and of {n}..{1}, continuing with {0} *)
  | And
  (* returns the boolean or of {n}..{1}, continuing with {0} *)
  | Or
  (* returns the boolean xor of {n}..{1}, continuing with {0} *)
  | Xor
  (* polymorphic equality *)
  (* returns the structural equality of {2} and {1}, continuing with {0} *)
  | Eq
  (* returns the structural inequality of {2} and {1}, continuing with {0} *)
  | Neq
  (* numeric comparisons *)
  (* returns the numeric comparion of {2} and {1}, continuing with {0} *)
  | Gt
  (* returns the numeric comparion of {2} and {1}, continuing with {0} *)
  | Gte
  (* returns the numeric comparion of {2} and {1}, continuing with {0} *)
  | Lt
  (* returns the numeric comparion of {2} and {1}, continuing with {0} *)
  | Lte
  (* numeric operations *)
  (* returns the numeric addition of {n}..{1}, continuing with {0} *)
  | Add
  (* if n = 2,
     then returns the numeric negation of {1}
     else returns {n} - {n - 1}..{1}
     continuing with {0} *)
  | Sub
  (* returns the numeric multiplication of {n}..{1}, continuing with {0} *)
  | Mul
  (* returns {2} / {1}, continuing with {0} *)
  | Div
  (* returns {2} mod {1}, continuing with {0} *)
  | Mod

(* each expression pushes its return value onto the 'stack' *)
and expr =
  (* halt with exit code *)
  | Halt of value
  (* apply procedure to arguments *)
  | Apply of value * value list
  (* branching,
     switches on value,
     continuing with one of the cases,
     or the default case *)
  | Switch of value * expr list * expr
  (* constructs a set of mutually recursive objects *)
  | FixIntro of int * expr
  (* sets fix to value, and continues with k *)
  | FixCons of value * value * expr
  (* prints the values returned by expr *)
  | Debug of Syntax.expression * value list * expr

let rec expr_to_string (expr : expr) (tail : string list) : string list =
  match expr with
  | Halt arg -> "halt " :: value_to_string arg tail
  | Apply (fn, args) ->
      "(" :: value_to_string fn (values_to_string args (")" :: tail))
  | Switch (arg, cases, default) ->
      let rec cases_to_string cases =
        match cases with
        | [] -> expr_to_string default ("]" :: tail)
        | case :: cases -> expr_to_string case (" " :: cases_to_string cases)
      in
      "[switch " :: value_to_string arg (" " :: cases_to_string cases)
  | FixIntro (n, expr) ->
      "[fix " :: Int.to_string n :: " " :: expr_to_string expr ("]" :: tail)
  | FixCons (fix, value, k) ->
      "[cons! "
      :: value_to_string fix
           (" " :: value_to_string value (" " :: expr_to_string k ("]" :: tail)))
  | Debug (syntax, values, expr) ->
      "?["
      :: Syntax.expr_to_string syntax
      :: values_to_string values (" " :: expr_to_string expr ("]" :: tail))

and value_to_string (value : value) (tail : string list) : string list =
  match value with
  | Var i -> "{" :: Int.to_string i :: "}" :: tail
  | Unbound sym -> sym :: tail
  | Unit -> "unit" :: tail
  | Bool true -> "true" :: tail
  | Bool false -> "false" :: tail
  | Int i -> Int.to_string i :: tail
  | Real r -> Float.to_string r :: tail
  | String s -> "\"" :: s :: "\"" :: tail
  | Procedure (Expr expr) -> "[fn " :: expr_to_string expr ("]" :: tail)
  | Procedure Halt -> "halt" :: tail
  | Procedure Cons -> "cons" :: tail
  | Procedure Proj -> "proj" :: tail
  | Procedure Not -> "!" :: tail
  | Procedure And -> "&" :: tail
  | Procedure Or -> "|" :: tail
  | Procedure Xor -> "^" :: tail
  | Procedure Eq -> "=" :: tail
  | Procedure Neq -> "!=" :: tail
  | Procedure Gt -> ">" :: tail
  | Procedure Gte -> ">=" :: tail
  | Procedure Lt -> "<" :: tail
  | Procedure Lte -> "<=" :: tail
  | Procedure Add -> "+" :: tail
  | Procedure Sub -> "-" :: tail
  | Procedure Mul -> "*" :: tail
  | Procedure Div -> "/" :: tail
  | Procedure Mod -> "%" :: tail

(* TODO: this adds an extra trailing space *)
and values_to_string values tail =
  let rec values_to_string values =
    match values with
    | [] -> tail
    | arg :: args -> " " :: value_to_string arg (values_to_string args)
  in
  match values with [] -> tail | args -> values_to_string args
