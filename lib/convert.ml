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

type binding =
  | AnonVar
  | Var of string * CPS.ty
  | Const of string * CPS.ty * value

type arg_binding = Arg of string * CPS.ty

let args_to_types (arg_bindings : arg_binding list) =
  List.map (fun (Arg (_, ty)) -> ty) arg_bindings

type env = { vars : binding list; types : (string * CPS.ty) list }

(* TODO: church encode this *)
and meta_value =
  | Value of value
  | Closure of { bindings : arg_binding list; env : env; body : Syntax.t }

let bind_anon (env : env) : env = { env with vars = AnonVar :: env.vars }

let bind_var (Arg (sym, ty) : arg_binding) (env : env) : env =
  { env with vars = Var (sym, ty) :: env.vars }

let bind_const (Arg (sym, ty) : arg_binding) (value : value) (env : env) : env =
  { env with vars = Const (sym, ty, value) :: env.vars }

let bind_ty_var (sym : string) (env : env) : env = failwith "TODO"
let bind_ty (sym : string) (ty : CPS.ty) (env : env) : env = failwith "TODO"

let lookup (sym : string) (env : env) : value =
  let rec determine_index i vars : value =
    match vars with
    (* TODO: convert syms to ints / reals *)
    | [] -> Unbound sym
    | Var (sym', ty) :: _ when sym = sym' -> Var i
    (* correct a constant pointer by adding the future bindings to it *)
    | Const (sym', ty, Var i') :: _ when sym = sym' -> Var (i + i')
    | Const (sym', ty, value) :: _ when sym = sym' -> value
    | (AnonVar | Var _) :: vars -> determine_index (i + 1) vars
    | Const _ :: vars -> determine_index i vars
  in
  determine_index 0 env.vars

let lookup_ty (sym : string) (env : env) : CPS.ty =
  let rec determine_index i tys : CPS.ty =
    match tys with
    | [] -> Unbound sym
    (* TODO: i don't even know if this is correct anymore :sob: *)
    | (sym', (Var i' : CPS.ty)) :: _ when sym = sym' -> Var (i + i')
    | (sym', ty) :: _ when sym = sym' -> ty
    | _ :: tys -> determine_index (i + 1) tys
  in
  determine_index 0 env.types

let rec eval_ty (ty : Syntax.t) (env : env) : CPS.ty =
  match ty with
  | Expr (None, Round, Sym constructor :: args) ->
      (* determine the type constructor *)
      let constructor = lookup_ty constructor env in
      Construction (constructor, List.map (fun arg -> eval_ty arg env) args)
  | Sym ty -> lookup_ty ty env
  | _ -> Invalid

let base_env : env =
  {
    types =
      [
        ("@!", Never);
        ("@unit", Unit);
        ("@bool", Bool);
        ("@int", Int);
        ("@real", Real);
        ("@*", Product);
        ("@fn", Function);
      ];
    vars =
      [
        (* builtin procedures *)
        Const ("cons", Unknown, Procedure Cons);
        Const ("proj", Unknown, Procedure Proj);
        Const ("!", Unknown, Procedure Not);
        Const ("&", Unknown, Procedure And);
        Const ("|", Unknown, Procedure Or);
        Const ("^", Unknown, Procedure Xor);
        Const ("=", Unknown, Procedure Eq);
        Const ("!=", Unknown, Procedure Neq);
        Const (">", Unknown, Procedure Gt);
        Const (">=", Unknown, Procedure Gte);
        Const ("<", Unknown, Procedure Lt);
        Const ("<=", Unknown, Procedure Lte);
        Const ("+", Unknown, Procedure Add);
        Const ("-", Unknown, Procedure Sub);
        Const ("*", Unknown, Procedure Mul);
        Const ("/", Unknown, Procedure Div);
        Const ("%", Unknown, Procedure Mod);
      ];
  }

type meta_expr = meta_value -> expr

(** { [x @int] @[@t] [y @real] z ... body }
    matches a list of function arguments followed by a body,
    used for fn forms *)
let rec fn_clauses (env : env) (clauses : Syntax.t list) :
    (env * arg_binding list * Syntax.t) option =
  match clauses with
  (* no clauses is not allowed *)
  | [] -> None
  | [ body ] -> Some (env, [], body)
  (* a type argument *)
  | Expr (Some "@", Square, [ Sym sym ]) :: clauses ->
      fn_clauses (bind_ty_var sym env) clauses
  (* an argument binding with type *)
  | Expr (None, Square, [ Sym sym; ty ]) :: clauses ->
      Option.(
        let+ env, arg_bindings, body = fn_clauses env clauses in
        (env, Arg (sym, eval_ty ty env) :: arg_bindings, body))
  (* an un-typed argument *)
  | Sym sym :: clauses ->
      Option.(
        let+ env, arg_bindings, body = fn_clauses env clauses in
        (env, Arg (sym, (Unknown : CPS.ty)) :: arg_bindings, body))
  (* some invalid clause form *)
  | _ -> None

(** { [x @int] [y @real] z ... }
    matches a list of function arguments, not followed by a body,
    used for fn forms *)
let rec let_fn_clauses (env : env) (clauses : Syntax.t list) :
    (env * arg_binding list) option =
  match clauses with
  (* no arguments *)
  | [] -> Some (env, [])
  (* a type argument *)
  | Expr (Some "@", Square, [ Sym sym ]) :: clauses ->
      let_fn_clauses (bind_ty_var sym env) clauses
  (* an argument binding with type *)
  | Expr (None, Square, [ Sym sym; ty ]) :: clauses ->
      Option.(
        let+ env, arg_bindings = let_fn_clauses env clauses in
        (env, Arg (sym, eval_ty ty env) :: arg_bindings))
  (* an un-typed argument *)
  | Sym sym :: clauses ->
      Option.(
        let+ env, arg_bindings = let_fn_clauses env clauses in
        (env, Arg (sym, (Unknown : CPS.ty)) :: arg_bindings))
  (* some invalid clause form *)
  | _ -> None

type let_binding =
  | LetValue of string * Syntax.t
  | LetProcedure of string * arg_binding list * Syntax.t

(** { [a 10.0]
      [(x [z @real]) (+ z a)]
      [(y [z @real]) (+ (x a) z)]
      [b (y 1.0)]
      ...
      body }
    matches a list of bindings and body,
    used for let and define forms *)
let rec let_clauses (env : env) (clauses : Syntax.t list) :
    (env * let_binding list * Syntax.t) option =
  match clauses with
  (* no clauses is not allowed *)
  | [] -> None
  (* don't care what the body is *)
  | [ body ] -> Some (env, [], body)
  (* a value binding *)
  | Expr (None, Square, [ Sym sym; value_body ]) :: clauses ->
      Option.(
        let+ env, let_bindings, body = let_clauses env clauses in
        (env, LetValue (sym, value_body) :: let_bindings, body))
  (* a function binding *)
  | Expr (None, Square, [ Expr (None, Round, Sym sym :: fn_args); fn_body ])
    :: clauses ->
      Option.(
        let* env, arg_bindings = let_fn_clauses env fn_args in
        let+ env, let_bindings, body = let_clauses env clauses in
        (env, LetProcedure (sym, arg_bindings, fn_body) :: let_bindings, body))
  (* some invalid clause form *)
  | _ -> None

(** top-level convert *)
let rec cps (syn : Syntax.t) (env : env) (k : meta_expr) : expr =
  match syn with
  | Sym sym -> k @@ Value (lookup sym env)
  | String (None, str) -> k @@ Value (String str)
  (* [let
       [n 10]
       [(x z) (+ 1 (y z))]
       [(y [z @int]) (x z)]
       ...
       body]
     mutually recursive binding *)
  | Expr (None, Square, Sym "let" :: clauses) -> (
      match let_clauses env clauses with
      (* some alternate closure / nested closure? *)
      | Some (env, bindings, body) -> cps_let bindings env body k
      | None -> Invalid)
  (* [define
       [n (x 10)]
       [x z (+ 1 (y z))]
       [y [z @int] (x z)]
       ...
       body]
     mutually recursive function binding *)
  | Expr (None, Square, Sym "define" :: clauses) -> (
      match let_clauses env clauses with
      (* some alternate closure / nested closure? *)
      | Some (env, bindings, body) -> cps_define bindings env body k
      | None -> Invalid)
  (* [fn [x @int] [y @real] z ... body] function introduction *)
  | Expr (None, Square, Sym "fn" :: clauses) -> (
      match fn_clauses env clauses with
      | Some (env, bindings, body) -> k @@ Closure { bindings; env; body }
      | None -> Invalid)
  (* [match x
            ;; matches 1
            [1 (+ 1 2)]
            ;; matches 2
            [2 (+ 2 1)]
            ;; default (optional if exhaustive)
            3]
            simple pattern matching (doesn't support nesting) *)
  | Expr (None, Square, Sym "match" :: clauses) -> failwith "TODO"
  (* (f x y z) function application *)
  | Expr (None, Round, fn :: args) -> cps fn env @@ cps_args args env k
  | _ -> Invalid

(** converts a meta_value to a value *)
and bless_value (v : meta_value) : value =
  match v with
  | Value v -> v
  | Closure { bindings; env; body } -> (
      (* the args are passed left to right *)
      let env =
        (* bind the continuation address as an additional argument *)
        bind_anon
        @@ List.fold_left (fun env arg -> bind_var arg env) env bindings
      in
      let args_size = List.length bindings in
      (* the expected list of arguments for n-reduction *)
      (* this looks like [ ...; 3; 2; 1; 0; ] *)
      let n_args =
        List.mapi (fun i _ -> (Var (args_size - i - 1) : value)) bindings
      in
      let expr =
        (* close the body *)
        cps body env
        (* continue by calling the passed continuation, bound to 0 *)
        @@ fun arg -> Apply (Var 0, [ bless_value arg ])
      in
      match expr with
      (* if we just apply the args in order, n-reduce fn *)
      | Apply (fn, args) when args = n_args -> fn
      (* otherwise, capture *)
      | _ -> Procedure (Expr (args_to_types bindings, expr)))

and bless_expr (k : meta_expr) : value =
  let expr = k @@ Value (Var 0) in
  match expr with
  | Halt (Var 0) -> Procedure Halt
  | Apply (fn, [ Var 0 ]) -> fn
  (* TODO: how does this work with assert? *)
  | _ -> Procedure (Expr ([ Unknown ], expr))

(** constructs a meta-expr for a function that continues with k *)
and cps_fn (fn : meta_value) (k : meta_expr) (args : meta_value list) : expr =
  match fn with
  | Value fn ->
      let args = List.map bless_value args in
      let k = bless_expr k in
      (* we pass k as the last parameter *)
      Apply (fn, args @ [ k ])
  | Closure { bindings; env; body } ->
      (* TODO:
         if a sym is only used once in the body, then we can inline it
         regardless of size,
         the reference implementation cheats to determine this,
         so i've left it off at the moment *)
      (* bless the args *)
      let args = List.map bless_value args in
      (* the args are passed left to right *)
      let rec bind_args bindings args env =
        match (bindings, args) with
        (* if we ran out of bindings,
           then we ignore the rest of the arguments,
           this is invalid if there are more arguments,
           but w/e *)
        | [], _ -> env
        (* if there are bindings but no values,
           we just apply the binding,
           this is also invalid, but w/e *)
        | binding :: bindings, [] ->
            bind_args bindings [] @@ bind_var binding env
        (* if there is both a sym and an arg,
           we figure out if we should inline the argument or not *)
        | sym :: syms, arg :: args ->
            if CPS.small_value arg
            (* if the argument is small
               then we are happy to inline it,
               regardless of duplication *)
            then bind_args syms args @@ bind_const sym arg env
            (* otherwise we bind it as an argument *)
              else bind_args syms [] @@ bind_var sym env
      in
      let env = bind_args bindings args env in
      let expr = cps body env k in
      Apply (Procedure (Expr (args_to_types bindings, expr)), args)

(** constructs a meta-expr for a list of args *)
and cps_args (arg_syns : Syntax.t list) (env : env) (k : meta_expr)
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
    (fun arg_syn k -> fun args -> cps arg_syn env @@ fun arg -> k (arg :: args))
    arg_syns fn []

and cps_let (let_bindings : let_binding list) (env : env) (body : Syntax.t)
    (k : meta_expr) : expr =
  (* fold each binding so that they are evaluated and bound in order
     at the end we pass the values to k *)
  let folding =
    List.fold_right
      (fun let_binding k ->
        match let_binding with
        | LetValue (sym, syntax) ->
            (* to 'let' a value
               we cps convert it syntax,
               passing it into the env,
               and passing it to k *)
            fun env ->
              (* cps convert the value *)
              cps syntax env
              (* and bind the resulting value into the env, passing it on *)
              @@ fun arg ->
              let env = bind_const (Arg (sym, Unknown)) (bless_value arg) env in
              k env
        | LetProcedure (sym, bindings, body) ->
            (* to 'let' a function
               we bless it as a closure into the env,
               continuing to k *)
            fun env ->
              let env =
                bind_const
                  (* TODO: better typing *)
                  (Arg (sym, Unknown))
                  (bless_value @@ Closure { bindings; env; body })
                  env
              in
              k env)
      let_bindings
  in
  folding (fun env -> cps body env k) env

(** converts mutually recursive functions *)
and cps_define (definitions : let_binding list) (env : env) (body : Syntax.t)
    (k : meta_expr) : expr =
  (* map the let bindings to environment bindings *)
  let bindings =
    (* split the bindings so to bind the functions, then the values *)
    let procs, values =
      List.fold_right
        (function
          | LetProcedure (sym, arg_bindings, _) ->
              fun (procs, values) ->
                let arg_tys = args_to_types arg_bindings in
                let (ty : CPS.ty) =
                  Construction (Function, arg_tys @ [ Unknown ])
                in
                (Arg (sym, ty) :: procs, values)
          | LetValue (sym, _) ->
              fun (procs, values) -> (procs, Arg (sym, Unknown) :: values))
        definitions ([], [])
    in
    (* join them in order *)
    procs @ values
  in
  (* then bind the values in order of declaration *)
  let env = List.fold_left (fun env arg -> bind_var arg env) env bindings in
  (* next, bless each value in the same order *)
  let k =
    (* bless each value and procedure in order,
       splitting into procedures and values
       so that all procedures are done before all values
       and setting the corresponding field between each value *)
    let procs, values =
      List.fold_right
        (function
          | LetValue (_, body) ->
              fun (procs, values) ->
                let value i k =
                  let k' arg : expr =
                    let arg = bless_value arg in
                    FixSet (i, arg, values (i - 1) k)
                  in
                  cps body env k'
                in
                (procs, value)
          | LetProcedure (_, arg_bindings, body) ->
              fun (procs, values) ->
                let proc = bless_value @@ Closure { bindings; env; body } in
                let proc i k : expr = FixSet (i, proc, procs (i - 1) k) in
                (proc, values))
        definitions
        (* identity / return *)
        ((fun i k -> k i), fun i k -> k)
    in
    procs (List.length definitions - 1) @@ fun i -> values i @@ cps body env k
  in
  FixIntro (List.length definitions, k)

and cps_switch (value : meta_value) (branches : Syntax.t list)
    (default : Syntax.t) (env : env) (k : meta_expr) : expr =
  (* bless argument *)
  let value = bless_value value in
  (* bless continuation address *)
  let k = bless_expr k in
  let env = bind_anon env in
  (* re-lift the blessed address to a meta-expr *)
  let (k : meta_expr) = fun arg -> Apply (k, [ bless_value arg ]) in
  (* convert each branch to continue by calling the continuation address *)
  let branches = List.map (fun branch -> cps branch env k) branches in
  (* and the default branch *)
  let default = cps default env k in
  (* construct the switch *)
  Switch (value, branches, default)

let ty_cons_clauses (clause : Syntax.t) =
  match clause with
  | Expr (None, Round, Sym sym :: args) -> (
      let rec args_clauses (clauses : Syntax.t list) =
        match clauses with
        | Sym arg :: clauses ->
            Option.(
              let+ args = args_clauses clauses in
              arg :: args)
        | _ -> None
      in
      match args_clauses args with
      | Some args -> Some (sym, Some args)
      | None -> None)
  | Sym sym -> Some (sym, None)
  | _ -> None

let entry_point : Syntax.t = Expr (None, Round, [ Sym "main" ])

(** module is the top-level cps conversion for a file

    a module is made up of function,
    constant,
    and type declarations,
    its like a top-level define,
    but also supports types

    at the moment, modules expect to call a function named "main"
    that has no arguments

    each function definition looks like this:
      [(f [a @int] b...) (+ a b)]

    and types look like this:
      @[(@list @a)
        (cons @a (@list @a))
        nil]

      ;; type aliases are introduced using @=, rather than @
      @=[@number @int]

      @[@m m] *)
let cps_module (clauses : Syntax.t list) : expr =
  let rec module_clauses (clauses : Syntax.t list) (env : env) :
      (env * (env -> env * let_binding list)) option =
    match clauses with
    | [] -> Some (env, fun env -> (env, []))
    (* a type construction *)
    | Expr (Some "@", Square, ty_cons :: value_conses) :: clauses ->
        Option.(
          (* TODO: bind the object constructor functions *)
          let* sym, args = ty_cons_clauses ty_cons in
          let env = bind_ty_var sym env in
          let+ env, f = module_clauses clauses env in
          (* TODO: actually bind the constructor *)
          let (ty : CPS.ty) = Never in
          let bind env = f @@ bind_ty sym ty env in
          (env, bind))
    (* a type alias *)
    | Expr (Some "@=", Square, [ Sym sym; alias ]) :: clauses ->
        Option.(
          let env = bind_ty_var sym env in
          let+ env, f = module_clauses clauses env in
          let ty = eval_ty alias env in
          let bind env = f @@ bind_ty sym ty env in
          (env, bind))
    (* a value binding *)
    | Expr (None, Square, [ Sym sym; body ]) :: clauses ->
        Option.(
          let+ env, let_bindings = module_clauses clauses env in
          let bind env =
            let env, let_bindings = let_bindings env in
            (env, LetValue (sym, body) :: let_bindings)
          in
          (env, bind))
    (* a function binding *)
    | Expr (None, Square, [ Expr (None, Round, Sym sym :: fn_args); fn_body ])
      :: clauses ->
        Option.(
          let* env, arg_bindings = let_fn_clauses env fn_args in
          let+ env, let_bindings = module_clauses clauses env in
          let bind env =
            let env, let_bindings = let_bindings env in
            (env, LetProcedure (sym, arg_bindings, fn_body) :: let_bindings)
          in
          (env, bind))
    (* some invalid clause form *)
    | _ -> None
  in
  match module_clauses clauses base_env with
  | Some (env, definitions) ->
      let env, definitions = definitions env in
      cps_define definitions env entry_point @@ fun arg ->
      Halt (bless_value arg)
  | None -> Invalid
