(** convert parsed direct-style chEVALier to the continuation passing style IR
*)
module Syntax = struct
  type brackets = Syntax.brackets
  type t = Syntax.expression

  let syntax_to_string = Syntax.expr_to_string
  let fn_sym : t = Sym "fn"

  (** [fn ...args body] *)
  let mk_fn (args : string list) (body : t) : t =
    Expr
      ( None,
        Square,
        fn_sym :: (List.map (fun arg -> (Sym arg : t)) args @ [ body ]) )

  (** (fn args...) *)
  let mk_apply (fn : t) (args : t list) : t = Expr (None, Round, fn :: args)
end

module CPS = struct
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

  let rec shift_value (shift : int) (value : value) : value =
    if shift = 0 then value
    else
      match value with
      | Var addr -> Var (addr + shift)
      | Procedure proc -> Procedure (shift_proc shift proc)
      | Unbound _ | Unit | Bool _ | Int _ | Real _ | String _ -> value

  and shift_proc (shift : int) (proc : proc) : proc =
    if shift = 0 then proc
    else match proc with Expr expr -> Expr (shift_expr shift expr) | _ -> proc

  and shift_expr (shift : int) (expr : expr) : expr =
    if shift = 0 then expr
    else
      match expr with
      | Halt v -> Halt (shift_value shift v)
      | Apply (fn, args) ->
          Apply (shift_value shift fn, List.map (shift_value shift) args)
      | Switch (v, branches, default) ->
          Switch
            ( shift_value shift v,
              List.map (shift_expr shift) branches,
              shift_expr shift default )
      | FixIntro (n, k) -> FixIntro (n, shift_expr shift k)
      | FixCons (fix, value, k) ->
          FixCons
            (shift_value shift fix, shift_value shift value, shift_expr shift k)
      | Debug (syntax, vs, k) ->
          Debug (syntax, List.map (shift_value shift) vs, shift_expr shift k)
end

type expr = Language.expr
type proc = Language.proc
type value = Language.value

type env_binding = Const of string * value | Binding of string option
and env = env_binding list

and meta_value =
  | Value of value
  | Closure of { formals : string list; env : env; body : Syntax.t }

type meta_expr =
  | Value of value
  (* number of bound arguments -> meta arguments -> expr *)
  | Abstract of (int -> meta_value list -> expr)

let resolve (env : env) (shift : int) (sym : string) : value =
  let rec lookup (env : env) (bindings : int) : value =
    match env with
    | [] -> (
        let to_int = int_of_string_opt sym in
        let to_real = float_of_string_opt sym in
        match (to_int, to_real) with
        | Some i, _ -> Int i
        | _, Some r -> Real r
        | _ -> Unbound sym)
    | Binding (Some sym') :: _ when sym = sym' -> Var (bindings - shift)
    | Binding _ :: env -> lookup env (bindings + 1)
    | Const (sym', value) :: _ when sym = sym' ->
        let value = CPS.shift_value (bindings - shift) value in
        value
    | Const _ :: env -> lookup env (bindings + 1)
  in
  lookup env 0

let bind_const (env : env) (sym : string) (value : value) =
  Const (sym, value) :: env

let bind_var (env : env) (sym : string option) = Binding sym :: env

(* clause pattern matching: *)

(** { x y z ... body }
    matches a list of function arguments followed by a body,
    used for fn forms *)
let rec fn_clauses (clauses : Syntax.t list) : string list * Syntax.t =
  match clauses with
  (* no clauses is not allowed *)
  | [] -> failwith "invalid fn form, missing body"
  | [ body ] -> ([], body)
  (* an argument *)
  | Sym sym :: clauses ->
      let syms, body = fn_clauses clauses in
      (sym :: syms, body)
  (* some invalid clause form *)
  | form :: _ -> failwith @@ "invalid fn form: " ^ Syntax.syntax_to_string form

(** { x y z ... }
    matches a list of function arguments, not followed by a body,
    used for let fn forms *)
let rec let_fn_clauses (clauses : Syntax.t list) : string list =
  match clauses with
  (* no arguments *)
  | [] -> []
  (* an argument *)
  | Sym sym :: clauses ->
      let arg_bindings = let_fn_clauses clauses in
      sym :: arg_bindings
  (* some invalid clause form *)
  | form :: _ ->
      failwith @@ "invalid let-fn form: " ^ Syntax.syntax_to_string form

type let_binding =
  | LetValue of string * Syntax.t
  | LetProcedure of string * string list * Syntax.t

(** { [a 10.0]
      [(x z) (+ z a)]
      [(y z) (+ (x a) z)]
      [b (y 1.0)]
      ...
      body }
    matches a list of bindings and body,
    used for let and define forms *)
let rec let_clauses (clauses : Syntax.t list) : let_binding list * Syntax.t =
  match clauses with
  (* no clauses is not allowed *)
  | [] -> failwith "invalid let form, missing body"
  (* don't care what the body is *)
  | [ body ] -> ([], body)
  (* a value binding *)
  | Expr (None, Square, [ Sym sym; value_body ]) :: clauses ->
      let let_bindings, body = let_clauses clauses in
      (LetValue (sym, value_body) :: let_bindings, body)
  (* a function binding *)
  | Expr (None, Square, [ Expr (None, Round, Sym sym :: fn_args); fn_body ])
    :: clauses ->
      let arg_bindings = let_fn_clauses fn_args in
      let let_bindings, body = let_clauses clauses in
      (LetProcedure (sym, arg_bindings, fn_body) :: let_bindings, body)
  (* some invalid clause form *)
  | form :: _ ->
      failwith @@ "invalid let binding form: " ^ Syntax.syntax_to_string form

let rec cps (syn : Syntax.t) (env : env) (shift : int) (k : meta_expr) : expr =
  match syn with
  | Sym sym -> cps_apply k shift [ Value (resolve env shift sym) ]
  | String (None, str) -> cps_apply k shift [ Value (String str) ]
  (* [fn x y z ... body] function introduction *)
  | Expr (None, Square, Sym "fn" :: clauses) ->
      let formals, body = fn_clauses clauses in
      cps_apply k shift @@ [ Closure { formals; env; body } ]
  (* (f x y z) function application *)
  | Expr (None, Round, fn :: args) ->
      cps fn env shift @@ abstract_args args env k
  (* [let [x 3] [(y z) (+ x z)] (y 10)] let forms *)
  | Expr (None, Square, Sym "let" :: clauses) ->
      let let_bindings, body = let_clauses clauses in
      abstract_let let_bindings env shift body k
  (* [define [x 3] [(y z) (+ x z)] (y 10)] mutually recursive define forms *)
  | Expr (None, Square, Sym "define" :: clauses) ->
      let let_bindings, body = let_clauses clauses in
      abstract_define let_bindings env shift body k
  (* ?[x] debug *)
  | Expr (Some "?", Square, [ expr ]) ->
      cps expr env shift
      @@ Abstract
           (fun shift args ->
             let k = cps_apply k shift args in
             Debug (expr, List.map (bless_value shift) args, k))
  | form -> failwith @@ "invalid form: " ^ Syntax.syntax_to_string form

and cps_apply (k : meta_expr) (shift : int) (args : meta_value list) : expr =
  match k with
  | Value v -> Apply (CPS.shift_value shift v, List.map (bless_value shift) args)
  | Abstract f -> f shift args

and bless_value (shift : int) (v : meta_value) : value =
  match v with
  | Value v -> CPS.shift_value shift v
  | Closure { formals; env; body } -> (
      let n = List.length formals in
      (* the args are passed left to right,
         so we bind them to var addresses such that
         the left-most formal is {n - 1}
         and the right-most formal is {0}
         we also add an additional formal {n} for the continuation *)
      let env =
        List.fold_left
          (fun env formal -> bind_var env @@ Some formal)
          (* reserve space for k *)
          (bind_var env None)
          formals
      in
      (* we expect that a n-redex will pass the arguments in order to some other
         function `fn`
         which looks like Apply (fn, [ ...; 3; 2; 1; 0; ]) *)
      let (nformals : value list) =
        Var n
        :: List.mapi (fun i _ -> (Var (n - i - 1 - shift) : value)) formals
      in
      let expr =
        (* close the body *)
        cps body env shift
        (* continue by calling the passed continuation, bound to n *)
        @@ Value (Var (n - shift))
      in
      match expr with
      (* if we just apply the args in order, n-reduce to fn *)
      | Apply (fn, formals) when formals = nformals -> fn
      (* otherwise, capture *)
      | _ -> Procedure (Expr expr))

(* this is incorrect, some number of values needs to be passed to f *)
and bless_expr (shift : int) (expr : meta_expr) : value =
  match expr with
  | Value v -> CPS.shift_value shift v
  | Abstract f ->
      let shift = shift + 1 in
      (* apply it to one value *)
      Procedure (Expr (f shift [ Value (Var (-shift)) ]))

and abstract_args (arg_syns : Syntax.t list) (env : env) (k : meta_expr) :
    meta_expr =
  Abstract
    (fun shift args ->
      match args with
      | [ fn ] ->
          let fn = abstract_apply fn k in
          (* we eval each arg in turn,
             continuting with (fn {n} ... {0})  *)
          (* this is complicated
             we fold up the arguments from right to left,
             which creates a left to right evaluation order,
             continuing with calling the function
             the function expects a list of arguments
             so we actually continue by re-accumulating the list
             in the meta expression *)
          let rec call args shift formals =
            match args with
            | [] -> cps_apply fn shift @@ List.rev formals
            | arg :: args ->
                cps arg env shift
                @@ Abstract
                     (fun shift formals' ->
                       match formals' with
                       | [ formal ] -> call args shift (formal :: formals)
                       | _ -> failwith "internal compiler error")
          in
          call arg_syns shift []
      | _ -> failwith "internal compiler error")

and abstract_apply (fn : meta_value) (k : meta_expr) : meta_expr =
  match fn with
  | Value fn ->
      Abstract
        (fun shift args ->
          let args = List.map (bless_value shift) args in
          let k = bless_expr shift k in
          (* we pass k as the first parameter *)
          Apply (CPS.shift_value shift fn, k :: args))
  | Closure { formals; env; body } ->
      Abstract
        (fun shift args ->
          (* TODO:
             if a sym is only used once in the body, then we can inline it
             regardless of size,
             the reference implementation cheats to determine this,
             so i've left it off at the moment *)
          (* bless the args *)
          let args = List.map (bless_value shift) args in
          (* the args are passed left to right, analyse them to bind them *)
          let rec bind_args formals' args' f =
            match (formals', args') with
            (* if we ran out of bindings,
               then we ignore the rest of the arguments,
               this is invalid if there are more arguments *)
            | [], [] -> ([], 0, f)
            | [], _ ->
                print_endline @@ "incorrect number of arguments";
                print_endline @@ "formals: " ^ String.concat " " formals;
                print_endline @@ "arguments: " ^ String.concat " "
                @@ Language.values_to_string args [];
                failwith "incorrect number of arguments"
            (* if there are bindings but no values,
               we just apply the binding,
               this is also invalid *)
            | _, [] ->
                print_endline @@ "incorrect number of arguments";
                print_endline @@ "formals: " ^ String.concat " " formals;
                print_endline @@ "arguments: " ^ String.concat " "
                @@ Language.values_to_string args [];
                failwith "incorrect number of arguments"
            (* if there is both a sym and an arg,
               we figure out if we should inline the argument or not *)
            | sym :: formals, arg :: args -> (
                match CPS.small_value arg with
                (* if the argument is small
                   then we are happy to inline it,
                   regardless of duplication *)
                | true ->
                    bind_args formals args @@ fun count env ->
                    let env = f count env in
                    bind_const env sym arg
                (* otherwise we bind it as a formal *)
                | false ->
                    (* record that we bound a variable,
                       in order to correct inlined vars *)
                    let bound_args, count, env' =
                      bind_args formals args @@ fun count env ->
                      let env = f count env in
                      bind_var env @@ Some sym
                    in
                    (arg :: bound_args, count + 1, env'))
          in
          let args, count, env' = bind_args formals args (fun _ env -> env) in
          let env = env' count env in
          let expr = cps body env shift k in
          Apply (Procedure (Expr expr), args))

and abstract_let (let_bindings : let_binding list) (env : env) (shift : int)
    (body : Syntax.t) (k : meta_expr) : expr =
  let rec solve bindings =
    match bindings with
    | [] -> body
    | LetValue (formal, value) :: bindings ->
        Syntax.(mk_apply (mk_fn [ formal ] @@ solve bindings) [ value ])
    | LetProcedure (formal, formals, body) :: bindings ->
        let proc = Syntax.mk_fn formals body in
        Syntax.(mk_apply (mk_fn [ formal ] @@ solve bindings) [ proc ])
  in
  let syntax = solve let_bindings in
  cps syntax env shift k

and abstract_define (definitions : let_binding list) (env : env) (shift : int)
    (body : Syntax.t) (k : meta_expr) : expr =
  (* split  *)
  let rec partition_defs definitions =
    match definitions with
    | [] -> ([], [])
    | LetValue (sym, syn) :: definitions ->
        let values, procs = partition_defs definitions in
        ((sym, syn) :: values, procs)
    | LetProcedure (sym, formals, body) :: definitions ->
        let values, procs = partition_defs definitions in
        (values, (sym, formals, body) :: procs)
  in
  let values, procs = partition_defs definitions in
  let n = List.length definitions in
  (* bind fix return address *)
  let env = bind_var env None in
  (* bind procs in env *)
  let env =
    List.fold_left (fun env (sym, _, _) -> bind_var env @@ Some sym) env procs
  in
  (* bind values in env *)
  let env =
    List.fold_left (fun env (sym, _) -> bind_var env @@ Some sym) env values
  in
  let rec fold_k' f vs k n' shift =
    match vs with
    | [] -> k n' shift
    | v :: vs ->
        (*
        f v shift
        @@ Abstract
             (fun shift args ->
               match args with
               | [ value ] ->
                   let value = bless_value shift value in
                   let k = fold_k' f vs k (n' + 1) shift in
                   FixCons (Var (shift - n), value, k)
               | _ -> failwith "internal compiler error")
               *)
        let v =
          f v shift
          (*
          @@ Value
               (Procedure
                  (Expr
                     (Apply
                        (Var (-shift), [ Var (-shift - 1); Var (-shift - 2) ]))))
                        *)
          @@ Abstract
               (fun shift' args ->
                 Apply
                   ( Var (shift' - shift + 1),
                     Var (shift' - shift) :: List.map (bless_value shift') args
                   ))
        in
        let k = fold_k' f vs k (n' - 1) shift in
        let (fixcons : expr) = FixCons (Var n', Var 0, Apply (Var 1, [])) in
        (Apply
           (Procedure (Expr v), [ Procedure (Expr fixcons); Procedure (Expr k) ])
          : expr)
  in

  (* bind cons return addresses *)
  let env' = bind_var env None in
  let env' = bind_var env' None in
  let values_k =
    fold_k'
      (fun (_, value_syn) shift k -> cps value_syn env' shift k)
      values
      (fun _ shift' ->
        cps body env shift'
        @@ Abstract
             (fun shift'' args ->
               Apply (Var (shift'' - shift), List.map (bless_value shift'') args)))
  in
  let proc_k =
    fold_k'
      (fun (_, formals, body) shift k ->
        cps_apply k shift @@ [ Closure { formals; env = env'; body } ])
      procs values_k (n + 1) (shift + n)
  in
  Apply (Procedure (Expr (FixIntro (n, proc_k))), [ bless_expr shift k ])

let base_env : env =
  [
    (* builtin procedures *)
    Const ("cons", Procedure Cons);
    Const ("proj", Procedure Proj);
    Const ("!", Procedure Not);
    Const ("&", Procedure And);
    Const ("|", Procedure Or);
    Const ("^", Procedure Xor);
    Const ("=", Procedure Eq);
    Const ("!=", Procedure Neq);
    Const (">", Procedure Gt);
    Const (">=", Procedure Gte);
    Const ("<", Procedure Lt);
    Const ("<=n", Procedure Lte);
    Const ("+", Procedure Add);
    Const ("-", Procedure Sub);
    Const ("*", Procedure Mul);
    Const ("/", Procedure Div);
    Const ("%", Procedure Mod);
    (* constants *)
    Const ("unit", Unit);
    Const ("true", Bool true);
    Const ("false", Bool false);
  ]

let rec module_clauses (clauses : Syntax.t list) : let_binding list =
  match clauses with
  | [] -> []
  (* a value binding *)
  | Expr (None, Square, [ Sym sym; value_body ]) :: clauses ->
      let module_bindings = module_clauses clauses in
      LetValue (sym, value_body) :: module_bindings
  (* a function binding *)
  | Expr (None, Square, [ Expr (None, Round, Sym sym :: fn_args); fn_body ])
    :: clauses ->
      let arg_bindings = let_fn_clauses fn_args in
      let module_bindings = module_clauses clauses in
      LetProcedure (sym, arg_bindings, fn_body) :: module_bindings
  (* some invalid clause form *)
  | form :: _ ->
      failwith @@ "invalid module form: " ^ Syntax.syntax_to_string form

let entry_point : Syntax.t = Expr (None, Round, [ Sym "main" ])

let cps_module (clauses : Syntax.t list) : expr =
  let definitions = module_clauses clauses in
  abstract_define definitions base_env 0 entry_point @@ Value (Procedure Halt)
