(** types *)
type ty =
  | Never
  | Unit
  | Bool
  | Int
  | Real
  | Product of ty list
  | Function of ty list * ty
  | Construction of ty list

(** expression-embedded values *)
and value =
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
  (* used in type checking *)
  | Assert of ty * expr
  (* apply procedure to arguments *)
  | Apply of value * value list
  (* branching *)
  | Switch of value * expr * expr list
  (* constructs a set of mutually recursive procedures *)
  | Fix of proc list * expr
  (* unconvertible expression *)
  | Invalid

(*
(** does type checking and inference using a bidirectional type checking system
    collects errors as it goes, type checking passes if no errors are collected

    TODO: need to properly track errors *)
module Types = struct
  type context = {
    procedures : (ty * ty) list;
    continuations : (ty * ty) list;
    values : ty list;
  }
  (** type checking function context *)

  (** looks up a procedure in the context *)
  let lookup_proc ctx (addr : proc_value) = List.nth ctx.procedures addr

  let lookup_cont ctx (addr : cont_value) = List.nth ctx.continuations addr

  (** looks up a value in the context *)
  let lookup_value ctx addr = List.nth ctx.values addr

  let append_value ctx value = { ctx with values = value :: ctx.values }

  (** type checking error types *)
  type err =
    | Unknown
    | Mismatch of { expected : ty; found : ty }
    | Cases of ty * expression list
    | Unbound of string

  type check_result = Errors of err list  (** result of type checking *)

  type infer_result = ty * check_result
  (** result of type checking *)

  (** monadic lifts for no errors *)
  let ok : check_result = Errors []

  (** monadic lifts for one error *)
  let err (err : err) : check_result = Errors [ err ]

  (** monadic product *)
  let ( @ ) (Errors a) (Errors b) = Errors (a @ b)

  let ( @^ ) a (ty, b) : infer_result = (ty, a @ b)

  (** monadic flatmap *)
  let ( let* ) ((a, a_errs) : infer_result) (f : ty -> infer_result) :
      infer_result =
    let b, b_errs = f a in
    (b, a_errs @ b_errs)

  (** monadic helper for infering, then checking *)
  let ( let$ ) ((a, a_errs) : infer_result) (f : ty -> check_result) :
      infer_result =
    let b_errs = f a in
    (a, a_errs @ b_errs)

  (** monadic map *)
  let ( let+ ) ((a, errs) : infer_result) (f : ty -> ty) : infer_result =
    (f a, errs)

  (** monadic lift for checking if two types are equal *)
  let check_eq expected found =
    if expected = found then ok else err @@ Mismatch { expected; found }

  (** type check expression *)
  let rec check_expr ctx ty expr : check_result =
    match expr with
    | Halt -> ok
    | Assert (ty', expr) ->
        (* check that the assertion checks *)
        check_expr ctx ty' expr
        (* check that the assertion matches the checked type *)
        @ check_eq ty ty'
    | Apply (proc, arg, k) ->
        (* lookup the cont in context *)
        let karg_ty, kret_ty = lookup_cont ctx k in
        (* lookup the proc in context *)
        let arg_ty, ret_ty = lookup_proc ctx proc in
        (* check that the argument matches *)
        check_value ctx arg_ty arg
        (* check that the continuation's argument type matches the procedure's
           return type *)
        @ check_eq karg_ty ret_ty
        (* check the continuation's return type *)
        @ check_eq ty kret_ty
    | Return (k, arg) ->
        let arg_ty, ret_ty = lookup_cont ctx k in
        check_value ctx arg_ty arg @ check_eq ty ret_ty
    | Switch (_, hd, tl) ->
        (*
           todo:
             atm this doesn't type check the value at all
             options are to either check that its one of the switchable types
             or to only accept ints
             or to not worry about it at this stage and we rely on an earlier
             stage to guarantee that the value and cases are correct (and thus
             probably an int, or value bound to an int)
        *)
        (* check head *)
        check_expr ctx ty hd
        (* check tail *)
        @ check_exprs ctx ty tl
    (* the rest of the expressions are primitive operations and each pushes
           a value to the ctx *)
    (* boolean not *)
    | Not (arg, k) ->
        (* check arg is correct *)
        check_value ctx Bool arg
        (* check k is correct *)
        @ check_expr (append_value ctx Bool) ty k
    (* boolean operations *)
    | And (a, b, k) | Or (a, b, k) | Xor (a, b, k) ->
        (* check arg a is correct *)
        check_value ctx Bool a
        (* check arg b is correct *)
        @ check_value ctx Bool b
        (* check k is correct *)
        @ check_expr (append_value ctx Bool) ty k
    (* int comparisons *)
    | Ieq (a, b, k)
    | Ineq (a, b, k)
    | Igt (a, b, k)
    | Igte (a, b, k)
    | Ilt (a, b, k)
    | Ilte (a, b, k) ->
        (* check arg a is correct *)
        check_value ctx Int a
        (* check arg b is correct *)
        @ check_value ctx Int b
        (* check k is correct *)
        @ check_expr (append_value ctx Bool) ty k
    (* int operations *)
    | Iadd (a, b, k)
    | Isub (a, b, k)
    | Imul (a, b, k)
    | Idiv (a, b, k)
    | Imod (a, b, k) ->
        (* check arg a is correct *)
        check_value ctx Int a
        (* check arg b is correct *)
        @ check_value ctx Int b
        (* check k is correct *)
        @ check_expr (append_value ctx Int) ty k
    (* real comparisons *)
    | Req (a, b, k)
    | Rneq (a, b, k)
    | Rgt (a, b, k)
    | Rgte (a, b, k)
    | Rlt (a, b, k)
    | Rlte (a, b, k) ->
        (* check arg a is correct *)
        check_value ctx Real a
        (* check arg b is correct *)
        @ check_value ctx Real b
        (* check k is correct *)
        @ check_expr (append_value ctx Bool) ty k
    (* real operations *)
    | Radd (a, b, k)
    | Rsub (a, b, k)
    | Rmul (a, b, k)
    | Rdiv (a, b, k)
    | Rmod (a, b, k) ->
        (* check arg a is correct *)
        check_value ctx Real a
        (* check arg b is correct *)
        @ check_value ctx Real b
        (* check k is correct *)
        @ check_expr (append_value ctx Real) ty k

  and check_exprs ctx ty exprs =
    match exprs with
    | [] -> ok
    | expr :: exprs -> check_expr ctx ty expr @ check_exprs ctx ty exprs

  (** type check value *)
  and check_value ctx ty (value : value) =
    match (ty, value) with
    (* look up the bound value, and check that they match *)
    | ty, Bound addr ->
        let ty' = lookup_value ctx addr in
        check_eq ty ty'
    (* unbound fails *)
    | ty, Unbound sym -> err @@ Unbound sym
    (* standard matchings *)
    | Unit, Unit -> ok
    | Bool, Bool _ -> ok
    | Int, Int _ -> ok
    | Real, Real _ -> ok
    (* check a and b components *)
    | Product (a_ty, b_ty), Record (a, b) ->
        check_value ctx a_ty a @ check_value ctx b_ty b
    | Function (arg, ret), Procedure label ->
        let arg', ret' = lookup_proc ctx label in
        (* check arg *)
        check_eq arg arg'
        (* check ret *)
        @ check_eq ret ret'
    | Construction tys, Construction (i, value) ->
        let ty = List.nth tys i in
        check_value ctx ty value
    | _ ->
        let inferred, errs = infer_value ctx value in
        errs @ check_eq ty inferred

  and infer_expr ctx expr : infer_result =
    match expr with
    | Halt -> (Never, ok)
    | Assert (ty, expr) ->
        (* switch to checking mode *)
        (ty, check_expr ctx ty expr)
    | Apply (proc, arg, k) ->
        (* lookup the cont in context *)
        let karg_ty, kret_ty = lookup_cont ctx k in
        (* lookup the proc in context *)
        let arg_ty, ret_ty = lookup_proc ctx proc in
        (* infer kret_ty *)
        ( kret_ty,
          (* check that the argument matches *)
          check_value ctx arg_ty arg
          (* check that the continuation's argument type matches the procedure's
           return type *)
          @ check_eq karg_ty ret_ty )
    | Return (k, arg) ->
        (* lookup the continuation in context *)
        let arg_ty, ret_ty = lookup_proc ctx k in
        (* check that the argument matches *)
        (* infer ret_ty *)
        (ret_ty, check_value ctx arg_ty arg)
    | Switch (_, hd, tl) ->
        (* infer hd *)
        let$ hd = infer_expr ctx hd in
        (* check tl against it *)
        check_exprs ctx hd tl
    (* boolean not *)
    | Not (arg, k) ->
        (* check arg is correct *)
        check_value ctx Bool arg
        (* infer k *)
        @^ infer_expr (append_value ctx Bool) k
    (* boolean operations *)
    | And (a, b, k) | Or (a, b, k) | Xor (a, b, k) ->
        (* check arg a is correct *)
        (check_value ctx Bool a
       (* check arg b is correct *)
       @ check_value ctx Bool b)
        (* infer k *)
        @^ infer_expr (append_value ctx Bool) k
    (* int comparisons *)
    | Ieq (a, b, k)
    | Ineq (a, b, k)
    | Igt (a, b, k)
    | Igte (a, b, k)
    | Ilt (a, b, k)
    | Ilte (a, b, k) ->
        (* check arg a is correct *)
        (check_value ctx Int a
       (* check arg b is correct *)
       @ check_value ctx Int b)
        (* infer k *)
        @^ infer_expr (append_value ctx Bool) k
    (* int operations *)
    | Iadd (a, b, k)
    | Isub (a, b, k)
    | Imul (a, b, k)
    | Idiv (a, b, k)
    | Imod (a, b, k) ->
        (* check arg a is correct *)
        (check_value ctx Int a
       (* check arg b is correct *)
       @ check_value ctx Int b)
        (* infer k *)
        @^ infer_expr (append_value ctx Int) k
    (* real comparisons *)
    | Req (a, b, k)
    | Rneq (a, b, k)
    | Rgt (a, b, k)
    | Rgte (a, b, k)
    | Rlt (a, b, k)
    | Rlte (a, b, k) ->
        (* check arg a is correct *)
        (check_value ctx Real a
       (* check arg b is correct *)
       @ check_value ctx Real b)
        (* infer k *)
        @^ infer_expr (append_value ctx Bool) k
    (* real operations *)
    | Radd (a, b, k)
    | Rsub (a, b, k)
    | Rmul (a, b, k)
    | Rdiv (a, b, k)
    | Rmod (a, b, k) ->
        (* check arg a is correct *)
        (check_value ctx Real a
       (* check arg b is correct *)
       @ check_value ctx Real b)
        (* infer k *)
        @^ infer_expr (append_value ctx Real) k

  (** type inference for a value *)
  and infer_value ctx value =
    match value with
    | Bound addr -> (lookup_value ctx addr, ok)
    | Unbound sym -> (Never, err @@ Unbound sym)
    | Unit -> (Unit, ok)
    | Bool _ -> (Bool, ok)
    | Int _ -> (Int, ok)
    | Real _ -> (Real, ok)
    | Record (a, b) ->
        let* a = infer_value ctx a in
        let+ b = infer_value ctx b in
        Product (a, b)
    | Procedure proc ->
        let arg_ty, ret_ty = lookup_proc ctx proc in
        (Function (arg_ty, ret_ty), ok)
    | Continuation cont ->
        let arg_ty, ret_ty = lookup_cont ctx cont in
        (Function (arg_ty, ret_ty), ok)
    | Construction (_, _) -> (Never, err @@ Unknown)
end
*)
