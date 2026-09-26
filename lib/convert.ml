(** convert parsed direct-style chEVALier to the continuation passing style IR
*)
module Syntax = struct
  type brackets = Syntax.brackets
  type t = Syntax.expression
end

module CPS = struct
  type ty = Language.ty
  type value = Language.value
  type proc_val = Language.proc_value
  type cont_val = Language.cont_value
  type expr = Language.expression
end

(* TODO *)
type context = { procedures : CPS.expr list; continuations : CPS.expr list }
(** stores procedures and continuations *)

type 'a lowered = context -> context * 'a

let return (x : 'a) : 'a lowered = fun ctx -> (ctx, x)

let ( let* ) (a : 'a lowered) (b : 'a -> 'b lowered) : 'b lowered =
 fun ctx ->
  let ctx, a = a ctx in
  b a ctx

let ( let+ ) (a : 'a lowered) (b : 'a -> 'b) : 'b lowered =
 fun ctx ->
  let ctx, a = a ctx in
  (ctx, b a)

let store_cont (k : CPS.expr) : CPS.cont_val lowered =
 fun ctx ->
  ( { ctx with continuations = k :: ctx.continuations },
    Label (List.length ctx.continuations) )

let store_proc (k : CPS.expr) : CPS.proc_val lowered =
 fun ctx ->
  ({ ctx with procedures = k :: ctx.procedures }, List.length ctx.procedures)

module SymMap = Map.Make (String)

type environment = { table : value SymMap.t; count : int }
(** mapping of symbols to cps values *)

(** abstract expressions *)
and expr =
  | Halt
  | Value of CPS.value
  | Function of { argument : Syntax.t; env : environment; k : expr }
  | Argument of { fn : value; k : expr }
(* TODO: need a switch abstraction *)

(** abstract values *)
and value =
  | Value of CPS.value
  | Closure of { sym : string; env : environment; body : Syntax.t }

let empty_env : environment = { table = SymMap.empty; count = 0 }
let lookup ({ table } : environment) (sym : string) = SymMap.find sym table

(** maps sym to the next position in the environment env *)
let bind_sym ({ table; count } : environment) (sym : string) :
    environment * CPS.value =
  let (v : CPS.value) = Var count in
  ({ table = SymMap.add sym (Value v : value) table; count = count + 1 }, v)

(** reserves a position in the environment *)
let bind_anonymous (env : environment) : environment * CPS.value =
  ({ env with count = env.count + 1 }, Var env.count)

(** binds value to sym in environment *)
let bind_const ({ table; count } : environment) (sym : string) (value : value) :
    environment =
  { table = SymMap.add sym value table; count = count + 1 }

let rec lower (syn : Syntax.t) (ctx : environment) (k : expr) : CPS.expr lowered
    =
  match syn with
  | Prefix (_, _) -> failwith "TODO"
  | Sym _ -> failwith "TODO"
  | String _ -> failwith "TODO"
  | Expr (_, _) -> failwith "TODO"

and lower_ret (k : expr) (a : value) : CPS.expr lowered =
  match k with
  (* directly lower a into halt *)
  | Halt ->
      let+ argument = lower_value a in
      (Halt argument : CPS.expr)
  (* directly lower a into continuing with v *)
  | Value v ->
      let+ argument = lower_value a in
      (Return (v, argument) : CPS.expr)
  (* we now have the function to fill the hole, so now lower the argument *)
  | Function { argument; env; k } ->
      lower argument env @@ Argument { fn = a; k }
  (* we now have the argument to fill the hole, so complete the call *)
  | Argument { fn; k } -> lower_call fn a k

and lower_call (f : value) (a : value) (k : expr) : CPS.expr lowered =
  match f with
  | Value f ->
      (* lower the argument *)
      let* a = lower_value a in
      (* lower the continuation *)
      let+ k = lower_expr k in
      (* apply f to a, continuing with k *)
      (Apply (f, a, k) : CPS.expr)
  | Closure { sym; env; body } -> (
      (* TODO: if sym is only used once in the body, then we can inline it
         regardless of size,
         the reference implementation cheats to determine if its only used once,
         so i've left it off at the moment *)
      let* argument = lower_value a in
      match argument with
      (* if the argument is trivial (a small constant)
         then we are happy to inline it, regardless of duplication *)
      | Var _ | Unit | Bool _ | Int _ | Real _ | Procedure _ | Continuation _ ->
          lower body (bind_const env sym (Value argument)) k
      (* otherwise, we lower the closure and then apply it to the value,
         making it a var *)
      (* TODO: this needs to closure convert correctly,
         which it doesn't at the moment *)
      | Record _ | Construction _ ->
          (* bind sym to a var *)
          let env, sym = bind_sym env sym in
          (* lower the closure, continuing with k directly *)
          let* cont = lower body env k in
          (* store the continuation *)
          let+ cont = store_cont cont in
          (* apply cont to argument *)
          (Return (Continuation cont, argument) : CPS.expr))

and lower_expr (k : expr) : CPS.value lowered =
  match k with
  (* halt or another trivial value can be returned immediately *)
  | Halt -> return (Continuation Halt : CPS.value)
  | Value v -> return v
  | _ ->
      (* lower the expression to be a continuation,
         binding the argument @ 0 *)
      let* k = lower_ret k @@ Value (Var 0) in
      (* store it and get the corresponding label *)
      let+ k = store_cont k in
      (* return the label as a cps value *)
      (Continuation k : CPS.value)

and lower_value (v : value) : CPS.value lowered =
  match v with
  | Value v -> return v
  (* TODO: this needs to closure convert correctly,
     which it doesn't at the moment *)
  | Closure { sym; env; body } -> (
      (* bind the argument as sym *)
      let env, sym = bind_sym env sym in
      (* bind the continuation to a var *)
      let env, k = bind_anonymous env in
      (* lower the closure, continuing with k *)
      let* proc = lower body env (Value k : expr) in
      match proc with
      (* TODO: maybe need to check number of references to sym here *)
      (* however, i feel like this does that automatically *)
      (* n reduction *)
      | Apply (proc, sym', k') when sym = sym' && k = k' -> return proc
      (* if cont does not n-reduce, then it needs to be stored *)
      | _ ->
          (* store it and get the corresponding label *)
          let+ proc = store_proc proc in
          (* return the label as a cps value *)
          (Procedure proc : CPS.value))
