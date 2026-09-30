type expr = Language.expr
type value = Language.value
type proc = Language.proc

type obj =
  | Unit
  | Bool of bool
  | Int of int
  | Real of float
  | String of string
  | Record of obj list
  | Closure of env * proc
  | Fix of obj option ref

and env = obj list

let rec object_to_string obj =
  match obj with
  | Unit -> "()"
  | Bool true -> "true"
  | Bool false -> "false"
  | Int i -> Int.to_string i
  | Real r -> Float.to_string r
  | String s -> String.concat "" [ "\""; s; "\"" ]
  | Record objs ->
      String.concat ""
        [ "("; String.concat " " @@ List.map object_to_string objs; ")" ]
  | Closure (_, Expr _) -> "<fn ...>"
  | Closure (_, Halt) -> "halt"
  | Closure (_, Cons) -> "cons"
  | Closure (_, Proj) -> "proj"
  | Closure (_, Not) -> "!"
  | Closure (_, And) -> "&"
  | Closure (_, Or) -> "|"
  | Closure (_, Xor) -> "^"
  | Closure (_, Eq) -> "="
  | Closure (_, Neq) -> "!="
  | Closure (_, Gt) -> ">"
  | Closure (_, Gte) -> ">="
  | Closure (_, Lt) -> "<"
  | Closure (_, Lte) -> "<="
  | Closure (_, Add) -> "+"
  | Closure (_, Sub) -> "-"
  | Closure (_, Mul) -> "*"
  | Closure (_, Div) -> "/"
  | Closure (_, Mod) -> "%"
  | Fix ref -> (
      match !ref with
      | Some obj -> object_to_string obj
      | None -> failwith "unbound fix")

exception InvalidProgram
exception UnboundSymbol of string

let lookup (env : env) (addr : int) : obj = List.nth env addr
let bind (env : env) (obj : obj) : env = obj :: env

let rec bind_fixes (env : env) (n : int) : env =
  if n = 0 then env else bind_fixes (Fix (ref None) :: env) @@ (n - 1)

let bind_fix (env : env) (addr : int) (obj : obj) : env =
  match lookup env addr with
  | Fix ref -> (
      match !ref with
      | None ->
          ref := Some obj;
          env
      | Some _ -> raise InvalidProgram)
  | _ -> raise InvalidProgram

let rec eval_expr (env : env) (expr : expr) : obj =
  match expr with
  | Halt arg -> (
      match eval_value env arg with
      | Unit -> exit 0
      | Int i -> exit i
      | _ -> raise InvalidProgram)
  | Assert (_, expr) -> eval_expr env expr
  | Apply (Procedure proc, args) -> eval_apply env proc args
  | Switch (arg, cases, default) -> (
      match (eval_value env arg, cases) with
      | Unit, [ case ] -> eval_expr env case
      | Unit, [] -> eval_expr env default
      | Bool true, [ case; _ ] -> eval_expr env case
      | Bool true, [ case ] -> eval_expr env case
      | Bool false, [ _; case ] -> eval_expr env case
      | Bool false, [ _ ] -> eval_expr env default
      | Bool _, [] -> eval_expr env default
      | Int i, cases -> (
          match List.nth_opt cases i with
          | Some case -> eval_expr env case
          | None -> eval_expr env default)
      | _ -> raise InvalidProgram)
  | FixIntro (n, expr) -> eval_expr (bind_fixes env n) expr
  | FixSet (addr, arg, expr) ->
      let value = eval_value env arg in
      eval_expr (bind_fix env addr value) expr
  | _ -> raise InvalidProgram

and eval_apply (env : env) (proc : proc) (args : value list) : obj =
  match (proc, args) with
  | Halt, [ arg ] -> (
      match eval_value env arg with
      | Unit -> exit 0
      | Int i -> exit i
      | _ -> raise InvalidProgram)
  | Cons, args ->
      let args, (env, proc) = eval_args_and_k env args in
      eval_apply (bind env @@ Record args) proc [ Var 0 ]
  | Proj, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ Int off; Record fields ] ->
          eval_apply (bind env @@ List.nth fields off) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | Not, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ Bool b ] -> eval_apply (bind env @@ Bool (not b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | And, args ->
      let args, (env, proc) = eval_args_and_k env args in
      let rec fold res args =
        match args with
        | [] -> res
        | Bool b :: args -> fold (res && b) args
        | _ -> raise InvalidProgram
      in
      let res =
        match args with
        | Bool res :: args -> fold res args
        | _ -> raise InvalidProgram
      in
      eval_apply (bind env @@ Bool res) proc [ Var 0 ]
  | Or, args ->
      let args, (env, proc) = eval_args_and_k env args in
      let rec fold res args =
        match args with
        | [] -> res
        | Bool b :: args -> fold (res || b) args
        | _ -> raise InvalidProgram
      in
      let res =
        match args with
        | Bool res :: args -> fold res args
        | _ -> raise InvalidProgram
      in
      eval_apply (bind env @@ Bool res) proc [ Var 0 ]
  | Xor, args ->
      let args, (env, proc) = eval_args_and_k env args in
      let rec fold res args =
        match args with
        | [] -> res
        | Bool b :: args -> fold (res <> b) args
        | _ -> raise InvalidProgram
      in
      let res =
        match args with
        | Bool res :: args -> fold res args
        | _ -> raise InvalidProgram
      in
      eval_apply (bind env @@ Bool res) proc [ Var 0 ]
  | Eq, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ a; b ] -> eval_apply (bind env @@ Bool (a = b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | Neq, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ a; b ] -> eval_apply (bind env @@ Bool (a <> b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | Gt, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ Int a; Int b ] -> eval_apply (bind env @@ Bool (a > b)) proc [ Var 0 ]
      | [ Real a; Real b ] ->
          eval_apply (bind env @@ Bool (a > b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | Gte, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ Int a; Int b ] ->
          eval_apply (bind env @@ Bool (a >= b)) proc [ Var 0 ]
      | [ Real a; Real b ] ->
          eval_apply (bind env @@ Bool (a >= b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | Lt, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ Int a; Int b ] -> eval_apply (bind env @@ Bool (a < b)) proc [ Var 0 ]
      | [ Real a; Real b ] ->
          eval_apply (bind env @@ Bool (a < b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | Lte, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ Int a; Int b ] ->
          eval_apply (bind env @@ Bool (a <= b)) proc [ Var 0 ]
      | [ Real a; Real b ] ->
          eval_apply (bind env @@ Bool (a <= b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | Add, args ->
      let args, (env, proc) = eval_args_and_k env args in
      let rec foldi res args =
        match args with
        | [] -> res
        | Int i :: args -> foldi (res + i) args
        | _ -> raise InvalidProgram
      in
      let rec foldr res args =
        match args with
        | [] -> res
        | Real r :: args -> foldr (Float.add res r) args
        | _ -> raise InvalidProgram
      in
      let res =
        match args with
        | Int res :: args -> Int (foldi res args)
        | Real res :: args -> Real (foldr res args)
        | _ -> raise InvalidProgram
      in
      eval_apply (bind env res) proc [ Var 0 ]
  | Sub, args ->
      let args, (env, proc) = eval_args_and_k env args in
      let rec foldi res args =
        match args with
        | [] -> res
        | Int i :: args -> foldi (res - i) args
        | _ -> raise InvalidProgram
      in
      let rec foldr res args =
        match args with
        | [] -> res
        | Real r :: args -> foldr (Float.sub res r) args
        | _ -> raise InvalidProgram
      in
      let res =
        match args with
        | [ Int res ] -> Int (-res)
        | [ Real res ] -> Real (Float.neg res)
        | Int res :: args -> Int (foldi res args)
        | Real res :: args -> Real (foldr res args)
        | _ -> raise InvalidProgram
      in
      eval_apply (bind env res) proc [ Var 0 ]
  | Mul, args ->
      let args, (env, proc) = eval_args_and_k env args in
      let rec foldi res args =
        match args with
        | [] -> res
        | Int i :: args -> foldi (res * i) args
        | _ -> raise InvalidProgram
      in
      let rec foldr res args =
        match args with
        | [] -> res
        | Real r :: args -> foldr (Float.mul res r) args
        | _ -> raise InvalidProgram
      in
      let res =
        match args with
        | Int res :: args -> Int (foldi res args)
        | Real res :: args -> Real (foldr res args)
        | _ -> raise InvalidProgram
      in
      eval_apply (bind env res) proc [ Var 0 ]
  | Div, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ Int a; Int b ] -> eval_apply (bind env @@ Int (a / b)) proc [ Var 0 ]
      | [ Real a; Real b ] ->
          eval_apply (bind env @@ Real (Float.div a b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | Mod, args -> (
      let args, (env, proc) = eval_args_and_k env args in
      match args with
      | [ Int a; Int b ] ->
          eval_apply (bind env @@ Int (a mod b)) proc [ Var 0 ]
      | [ Real a; Real b ] ->
          eval_apply (bind env @@ Real (Float.rem a b)) proc [ Var 0 ]
      | _ -> raise InvalidProgram)
  | _ -> raise InvalidProgram

and eval_args_and_k (env : env) (values : value list) : obj list * (env * proc)
    =
  match values with
  | [] -> raise InvalidProgram
  | [ k ] -> (
      match eval_value env k with
      | Closure (env, proc) -> ([], (env, proc))
      | _ -> raise InvalidProgram)
  | arg :: values ->
      let args, k = eval_args_and_k env values in
      (eval_value env arg :: args, k)

and eval_value (env : env) (value : value) : obj =
  match value with
  | Var addr -> lookup env addr
  | Unbound sym -> raise @@ UnboundSymbol sym
  | Unit -> Unit
  | Bool b -> Bool b
  | Int i -> Int i
  | Real r -> Real r
  | String s -> String s
  | Procedure proc -> Closure (env, proc)
