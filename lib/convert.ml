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
  type expr = Language.expression

  (** checks if a value is acceptably small to be duplicated *)
  let rec small_value (v : value) =
    match v with
    (* values like pointers, unit, bools, numbers are all small *)
    | Var _ | Unit | Bool _ | Int _ | Real _ -> true
    (* records are small if their contents are small *)
    | Record (car, cdr) -> small_value car && small_value cdr
    (* closures are not small *)
    | Procedure (Expr _) -> false
    (* builin procedures are small *)
    | Procedure Halt
    | Procedure Cons
    | Procedure Car
    | Procedure Cdr
    | Procedure Not
    | Procedure And
    | Procedure Or
    | Procedure Xor
    | Procedure Ieq
    | Procedure Ineq
    | Procedure Igt
    | Procedure Igte
    | Procedure Ilt
    | Procedure Ilte
    | Procedure Iadd
    | Procedure Isub
    | Procedure Imul
    | Procedure Idiv
    | Procedure Imod
    | Procedure Req
    | Procedure Rneq
    | Procedure Rgt
    | Procedure Rgte
    | Procedure Rlt
    | Procedure Rlte
    | Procedure Radd
    | Procedure Rsub
    | Procedure Rmul
    | Procedure Rdiv
    | Procedure Rmod ->
        true
    (* inductive types are not small *)
    | Construction (_, _) -> false
end

type cont = Language.expression
type value = Language.value

(* TODO:
   two parts:
     1. a constant map of sym -> meta_value
     2. a var indexing of sym list

   many functions will need an env added i suspect, or way to instruct it to add
   smth to the env ?? hard to tell... *)
type env

and meta_value =
  | Value of value
  | Closure of { syms : string list; env : env; body : Syntax.t }

(* TODO: do i need the binding? *)
(* TODO:
     of course not, the binding is always Var 0, i need to correct the
     rest of the bindings *)
(* TODO: need to shift the env by one! *)
let bind_sym (sym : string) (env : env) : env = failwith "TODO"
let bind_anon (env : env) : env = failwith "TODO"
let bind_const (sym : string) (arg : value) (env : env) : env = failwith "TODO"

type meta_cont = meta_value -> cont

(** top-level convert *)
let rec cps (syn : Syntax.t) (env : env) (k : meta_cont) : cont =
  match syn with
  | Prefix _ -> failwith "TODO"
  | Sym _ -> failwith "TODO"
  | String _ -> failwith "TODO"
  (* function application *)
  | Expr (Round, fn :: args) -> cps fn env @@ cps_args args env k
  | Expr _ -> failwith "TODO"

(* TODO:
   when you bless a closure, you need to shift the env by the number of
   bindings that have added since, in order to do this,
   you add a shift layer,
   that delays the shifting util you bless it
   i.e:
     Shift (Var 0) -> Var 1
     Shift (Shift (Var 0)) -> Var 2
     (* constants are unaffected *)
     Shift (Shift (Unit)) -> Unit
     Shift (Shift ({ syms, env, body })) ->
       bind 2 anonymous values to env first
       ...
   *)
(** converts a meta_value to a value *)
and bless_value (v : meta_value) : value =
  match v with
  | Value v -> v
  | Closure { syms; env; body } -> (

      (* the left most value gets the highest number *)
      let env = List.fold_left (fun env sym -> bind_sym sym env) env syms in
      let args_size = List.length syms in
      (* the expected list of arguments for n-reduction *)
      (* this looks like [ X ... 3; 2; 1; 0; ] *)
      let n_args =
        (Var args_size : value)
        :: List.mapi (fun i _ -> (Var (args_size - i - 1) : value)) syms
      in
      (* bind the continuation k to 0 *)
      let cont = cps body (bind_anon env) k in
      match cont with
      (* if we just apply the args in order, n-reduce fn *)
      | Apply (fn, args) when args = n_args -> fn
      (* otherwise, capture *)
      | _ -> Procedure (Expr cont))

and bless_cont (k : meta_cont) : value =
  (* TODO: need to shift the env by one? *)
  let cont = k @@ Value (Var 0) in
  match cont with
  | Halt (Var 0) -> Procedure Halt
  | Apply (fn, [ Var 0 ]) -> fn
  (* TODO: how does this work with assert? *)
  | _ -> Procedure (Expr cont)

(** constructs a meta-cont for a function that continues with k *)
and cps_fn (fn : meta_value) (k : meta_cont) : meta_value list -> cont =
  match fn with
  | Value fn ->
      fun args ->
        (* TODO:
           each value that we bless might change the number of bound
           values *)
        let args = List.map bless_value args in
        let k = bless_cont k in
        (* we pass k as the last parameter *)
        Apply (fn, args @ [ k ])
  | Closure { syms; env; body } ->
      fun args ->
        (* TODO:
         if sym is only used once in the body, then we can inline it
         regardless of size,
         the reference implementation cheats to determine this,
         so i've left it off at the moment *)
        (* bless the args *)
        let args = bless_value args in
        if CPS.small_value args
        (* if the argument is small
           then we are happy to inline it,
           regardless of duplication *)
        then cps body (bind_const sym args env) k
        (* otherwise, we lower the closure and then apply it to the value,
           making it a var *)
          else
          let cont = cps body (bind_sym sym env) k in
          (Apply (Procedure (Expr cont), args) : cont)

(** constructs a meta-cont for a list of args *)
and cps_args (syns : Syntax.t list) (env : env) (k : meta_cont)
    (fn : meta_value) : cont =
  let fn = cps_fn fn k in
  (* this is complicated
     we fold up the arguments from right to left,
     which creates a left to right evaluation order,
     continuing with calling the function
     the function expects a list of arguments
     so we actually continue by re-accumulating the list
     in the meta continuation *)
  List.fold_right
    (* arg syntax, meta continuation that takes a list of args *)
    (fun syn k -> fun args -> cps syn env @@ fun arg -> k (arg :: args))
    syns fn []

and cons (car : meta_value) (cdr : meta_value) : cont =
  let car = bless_value car in
  let cdr = bless_value cdr in
  Apply (Procedure Cons, [ car; cdr ])

and halt (arg : meta_value) : cont = Halt (bless_value arg)
(*
and k (arg : meta_value) : cont = Apply (Var 0, bless_value arg)
*)

(*
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

let rec lower
(syn : Syntax.t)
(env : environment)
(k : expr) : CPS.expr lowered
    =
  match syn with
  | Prefix (_, _) -> failwith "TODO"
  | Sym sym -> let v = lookup env sym in
      lower_ret k v
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
          *)
