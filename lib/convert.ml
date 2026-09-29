(** convert parsed direct-style chEVALier to the continuation passing style IR
*)
module Syntax = struct
  type brackets = Syntax.brackets
  type t = Syntax.expression
end

module CPS = struct
  type ty = Language.ty
  type value = Language.value
  type proc = Language.proc
  type expr = Language.expr

  (** checks if a value is acceptably small to be duplicated *)
  let rec small_value (v : value) =
    match v with
    (* values like pointers, unit, bools, numbers are all small *)
    | Var _ | Unbound _ | Unit | Bool _ | Int _ | Real _ -> true
    (* strings are not small *)
    | String _ -> false
    (* closures are not small *)
    | Procedure (Expr _) -> false
    (* builtin procedures are small *)
    | Procedure Halt
    | Procedure Cons
    | Procedure Proj
    | Procedure Not
    | Procedure And
    | Procedure Or
    | Procedure Xor
    | Procedure Eq
    | Procedure Neq
    | Procedure Gt
    | Procedure Gte
    | Procedure Lt
    | Procedure Lte
    | Procedure Add
    | Procedure Sub
    | Procedure Mul
    | Procedure Div
    | Procedure Mod ->
        true
end

module SymMap = Map.Make (String)

module Option = struct
  include Option

  let ( let* ) = bind
  let ( let+ ) o f = map f o
end

type expr = Language.expr
type value = Language.value

(* TODO:
   two parts:
     1. a constant map of sym -> meta_value
     2. a var indexing of sym list

   many functions will need an env added i suspect, or way to instruct it to add
   smth to the env ?? hard to tell... *)

type binding =
  | AnonVar
  | TypedVar of string * CPS.ty
  | UntypedVar of string
  | TypedConst of string * CPS.ty * value
  | UntypedConst of string * value
  | Type of string * CPS.ty

type env = binding list

and meta_value =
  | Value of value
  | Closure of { bindings : binding list; env : env; body : Syntax.t }
  | Fix of {
      procedures : (binding * binding list * Syntax.t) list;
      env : env;
      body : Syntax.t;
    }

let bind_sym (sym : string) (env : env) : env = UntypedVar sym :: env
let bind_anon (env : env) : env = AnonVar :: env

let bind_const (sym : string) (arg : value) (env : env) : env =
  UntypedConst (sym, arg) :: env

let rec shift_by (shift : int) (env : env) =
  if shift <= 0 then env else shift_by (shift - 1) @@ bind_anon env

let lookup sym env : value =
  let rec determine_index i vars : value =
    match vars with
    (* TODO: convert syms to chars / ints / reals *)
    | [] -> Unbound sym
    | TypedVar (sym', ty) :: _ when sym = sym' -> Var i
    | UntypedVar sym' :: _ when sym = sym' -> Var i
    | TypedConst (sym', ty, value) :: _ when sym = sym' -> value
    | UntypedConst (sym', value) :: _ when sym = sym' -> value
    | (AnonVar | TypedVar _ | UntypedVar _) :: vars ->
        determine_index (i + 1) vars
    | (TypedConst _ | UntypedConst _ | Type _) :: vars -> determine_index i vars
  in
  determine_index 0 env

(* TODO *)
let eval_ty (syn : Syntax.t) : CPS.ty = failwith "TODO"

let base_env : env =
  [
    (* builtin types *)
    Type ("@never", Never);
    Type ("@unit", Unit);
    Type ("@bool", Bool);
    Type ("@int", Int);
    Type ("@real", Real);
    (* builtin procedures *)
    (* TODO *)
    UntypedConst ("cons", Procedure Cons);
    UntypedConst ("proj", Procedure Proj);
    UntypedConst ("!", Procedure Not);
    UntypedConst ("&", Procedure And);
    UntypedConst ("|", Procedure Or);
    UntypedConst ("^", Procedure Xor);
    UntypedConst ("=", Procedure Eq);
    UntypedConst ("!=", Procedure Neq);
    UntypedConst (">", Procedure Gt);
    UntypedConst (">=", Procedure Gte);
    UntypedConst ("<", Procedure Lt);
    UntypedConst ("<=", Procedure Lte);
    UntypedConst ("+", Procedure Add);
    UntypedConst ("-", Procedure Sub);
    UntypedConst ("*", Procedure Mul);
    UntypedConst ("/", Procedure Div);
    UntypedConst ("%", Procedure Mod);
  ]

type meta_expr = meta_value -> expr

(** { [x @int] [y @real] z ... body } matches a list of function arguments and body *)
let rec fn_clauses (clauses : Syntax.t list) =
  match clauses with
  (* no clauses is not allowed *)
  | [] -> None
  (* don't care what the body is *)
  | [ body ] -> Some ([], body)
  (* an argument binding with type *)
  (* TODO: deal with types correctly *)
  | Expr (None, Square, [ Sym sym; ty ]) :: clauses ->
      Option.(
        let+ bindings, body = fn_clauses clauses in
        (TypedVar (sym, eval_ty ty) :: bindings, body))
  (* an un-typed argument *)
  | Sym sym :: clauses ->
      Option.(
        let+ bindings, body = fn_clauses clauses in
        (UntypedVar sym :: bindings, body))
  (* some invalid clause form *)
  | _ -> None

(** top-level convert *)
let rec cps (syn : Syntax.t) (env : env) (k : meta_expr) : expr =
  match syn with
  | Sym sym -> k @@ Value (lookup sym env)
  | String (None, str) -> k @@ Value (String str)
  (* [define [x z (+ 1 (y z))] [y [z @int] (x z)] ... body]
     mutually recursive function binding *)
  | Expr (None, Square, Sym "define" :: clauses) -> (
      let rec pmatch (clauses : Syntax.t list) =
        match clauses with
        (* no clauses is not allowed *)
        | [] -> None
        (* don't care what the body is *)
        | [ body ] -> Some ([], body)
        (* a function binding *)
        | Expr (None, Square, Sym sym :: fn_clauses') :: clauses ->
            Option.(
              let* fn_bindings, fn_body = fn_clauses fn_clauses' in
              let fn = (UntypedVar sym, fn_bindings, fn_body) in
              let+ bindings, body = pmatch clauses in
              (fn :: bindings, body))
        (* some invalid clause form *)
        | _ -> None
      in
      match pmatch clauses with
      (* some alternate closure / nested closure? *)
      | Some (procedures, body) -> k @@ Fix { procedures; env; body }
      | None -> Invalid)
  (* [fn [x @int] [y @real] z ... body] function introduction *)
  | Expr (None, Square, Sym "fn" :: clauses) -> (
      match fn_clauses clauses with
      | Some (bindings, body) -> k @@ Closure { bindings; env; body }
      | None -> Invalid)
  (* (f x y z) function application *)
  | Expr (None, Round, fn :: args) -> cps fn env @@ cps_args args env k
  | _ -> Invalid

(** converts a meta_value to a value *)
and bless_value (shift : int) (v : meta_value) : value =
  match v with
  | Value (Var n) -> (Var (n + shift) : value)
  | Value v -> v
  | Closure { bindings; env; body } -> (
      (* shift env *)
      let env = shift_by shift env in
      (* the args are passed left to right *)
      let env =
        bind_anon @@ List.fold_left (fun env sym -> bind_sym sym env) env syms
      in
      let args_size = List.length syms in
      (* the expected list of arguments for n-reduction *)
      (* this looks like [ ...; 3; 2; 1; 0; ] *)
      let n_args =
        List.mapi (fun i _ -> (Var (args_size - i - 1) : value)) syms
      in
      let expr =
        (* close the body *)
        cps body env
        (* continue by calling the passed continuation, bound to 0 *)
        @@ fun arg -> Apply (Var 0, [ bless_value 0 arg ])
      in
      match expr with
      (* if we just apply the args in order, n-reduce fn *)
      | Apply (fn, args) when args = n_args -> fn
      (* otherwise, capture *)
      | _ -> Procedure (Expr expr))
  | Fix _ -> _

and bless_expr (k : meta_expr) : value =
  let expr = k @@ Value (Var 0) in
  match expr with
  | Halt (Var 0) -> Procedure Halt
  | Apply (fn, [ Var 0 ]) -> fn
  (* TODO: how does this work with assert? *)
  | _ -> Procedure (Expr expr)

(** constructs a meta-expr for a function that continues with k *)
and cps_fn (fn : meta_value) (k : meta_expr) : meta_value list -> expr =
  match fn with
  | Value fn ->
      fun args ->
        let args = List.map (bless_value 0) args in
        let k = bless_expr k in
        (* we pass k as the last parameter *)
        Apply (fn, args @ [ k ])
  | Closure { bindings; env; body } ->
      fun args ->
        (* TODO:
         if a sym is only used once in the body, then we can inline it
         regardless of size,
         the reference implementation cheats to determine this,
         so i've left it off at the moment *)
        (* bless the args *)
        let args = List.map (bless_value 0) args in
        (* the args are passed left to right *)
        let rec bind_args syms args env =
          match (syms, args) with
          (* if we ran out of syms,
             then we ignore the rest of the arguments,
             this is invalid if there are more arguments,
             but w/e *)
          | [], _ -> env
          (* if there are syms but no values,
             we just bind the sym,
             this is also invalid, but w/e *)
          | sym :: syms, [] -> bind_args syms [] @@ bind_sym sym env
          (* if there is both a sym and an arg,
             we figure out if we should inline the argument or not *)
          (* TODO:
             INLINING POINTERS LIKE THIS IS NOT VALID,
             AS THEY ARE SHIFTED,
             THEY ALL NEED TO BE SHIFTED BY THE NUMBER OF ARGS
             THAT WILL BE BOUND IN THE END *)
          | sym :: syms, arg :: args ->
              if CPS.small_value arg
              (* if the argument is small
                 then we are happy to inline it,
                 regardless of duplication *)
              then bind_args syms args @@ bind_const sym arg env
              (* otherwise we bind it as an argument *)
                else bind_args syms [] @@ bind_sym sym env
        in
        let env = bind_args syms args env in
        let expr = cps body env k in
        Apply (Procedure (Expr expr), args)
  | Fix _ -> _

(** constructs a meta-expr for a list of args *)
and cps_args (syns : Syntax.t list) (env : env) (k : meta_expr)
    (fn : meta_value) : expr =
  let fn = cps_fn fn k in
  (* this is complicated
     we fold up the arguments from right to left,
     which creates a left to right evaluation order,
     continuing with calling the function
     the function expects a list of arguments
     so we actually continue by re-accumulating the list
     in the meta expression *)
  List.fold_right
    (* arg syntax, meta expression that takes a list of args *)
    (fun syn k -> fun args -> cps syn env @@ fun arg -> k (arg :: args))
    syns fn []

let cps syntax = cps syntax base_env @@ fun arg -> Halt (bless_value 0 arg)
