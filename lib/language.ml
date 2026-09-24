(* types *)
type ty =
  | Never
  | Unit
  | Bool
  | Int
  | Real
  | Product of ty * ty
  | Procedure of ty * ty
  | Construction of ty list

(* values *)
and value =
  | Bound of int
  | Unbound of string
  | Unit
  | Bool of bool
  | Int of int
  | Real of float
  | Record of value * value
  (* static function pointer *)
  | Label of label
  | Construction of int * value

(* special type for function addresses *)
and label = Label of int

(* each expression pushes its return value onto the 'stack' *)
and expression =
  (* used in type checking *)
  | Assert of ty * expression
  (* apply a procedure to value *)
  | Apply of label * value
  (* branching *)
  | Switch of value * expression * expression list
  (* bool operations *)
  | Not of value * expression
  | And of value * value * expression
  | Or of value * value * expression
  | Xor of value * value * expression
  (* int operations *)
  | Ieq of value * value * expression
  | Ineq of value * value * expression
  | Igt of value * value * expression
  | Igte of value * value * expression
  | Ilt of value * value * expression
  | Ilte of value * value * expression
  | Iadd of value * value * expression
  | Isub of value * value * expression
  | Imul of value * value * expression
  | Idiv of value * value * expression
  | Imod of value * value * expression
  (* real operations *)
  | Req of value * value * expression
  | Rneq of value * value * expression
  | Rgt of value * value * expression
  | Rgte of value * value * expression
  | Rlt of value * value * expression
  | Rlte of value * value * expression
  | Radd of value * value * expression
  | Rsub of value * value * expression
  | Rmul of value * value * expression
  | Rdiv of value * value * expression
  | Rmod of value * value * expression

(** does type checking and inference using a bidirectional type checking system
    collects errors as it goes, type checking passes if no errors are collected
*)
module Types = struct
  type context = { procedures : (ty * ty) list; values : ty list }
  (** type checking function context *)

  (** looks up a procedure in the context *)
  let lookup_proc ctx (Label addr : label) = List.nth ctx.procedures addr

  (** looks up a value in the context *)
  let lookup_value ctx addr = List.nth ctx.values addr

  let append_value ctx value =
    { procedures = ctx.procedures; values = value :: ctx.values }

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
    if expected == found then ok else err @@ Mismatch { expected; found }

  (** type check expression *)
  let rec check_expr ctx ty expr : check_result =
    match expr with
    | Assert (ty', expr) ->
        (* check that the assertion checks *)
        check_expr ctx ty' expr
        (* check that the assertion matches the checked type *)
        @ check_eq ty ty'
    | Apply (proc, arg) ->
        (* lookup the label in context *)
        let arg_ty, ret_ty = lookup_proc ctx proc in
        (* check that the argument matches *)
        check_value ctx arg_ty arg
        (* the return type *)
        @ check_eq ty ret_ty
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
    | Procedure (arg, ret), Label label ->
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
    | Assert (ty, expr) ->
        (* switch to checking mode *)
        (ty, check_expr ctx ty expr)
    | Apply (proc, arg) ->
        (* lookup the label in context *)
        let arg_ty, ret_ty = lookup_proc ctx proc in
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
    | Label label ->
        let arg_ty, ret_ty = lookup_proc ctx label in
        (Procedure (arg_ty, ret_ty), ok)
    | Construction (_, _) -> (Never, err @@ Unknown)
end
