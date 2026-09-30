type expr = Language.expr
type value = Language.value
type proc = Language.proc

exception InvalidProgram
exception UnboundSymbol of string

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
      | Some obj -> "!{ " ^ object_to_string obj ^ " }"
      | None -> "!{_}")

let rec dump_env (env : env) (n : int) (tail : string list) =
  match env with
  | [] -> tail
  | obj :: env ->
      dump_env env (n + 1)
        ((Int.to_string n ^ ": " ^ object_to_string obj) :: tail)

let dump_env (env : env) =
  print_endline "env:";
  print_endline @@ String.concat "\n" @@ dump_env env 0 []

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
      | Some obj ->
          print_endline @@ "attempted to set fix {" ^ Int.to_string addr
          ^ "}, however it was alread set to " ^ object_to_string obj;
          dump_env env;
          raise InvalidProgram)
  | obj ->
      print_endline @@ "attempted to set fix {" ^ Int.to_string addr
      ^ "}, didn't find a fix";
      dump_env env;
      raise InvalidProgram

let rec eval_expr (env : env) (expr : expr) : obj =
  match expr with
  | Halt arg -> (
      match eval_value env arg with
      | Unit -> exit 0
      | Int i -> exit i
      | _ -> raise InvalidProgram)
  | Assert (_, expr) -> eval_expr env expr
  | Apply (Procedure proc, args) ->
      eval_apply env proc @@ List.map (eval_value env) args
  | Apply (Var addr, args) ->
      let proc = lookup env addr in
      let args = List.map (eval_value env) args in
      let rec unwrap proc =
        match proc with
        | Closure (env, proc) -> eval_apply env proc args
        | Fix ref -> (
            match !ref with
            | Some proc -> unwrap proc
            | None -> raise InvalidProgram)
        | obj ->
            print_endline @@ "expected to find a proc {" ^ Int.to_string addr
            ^ "}, found " ^ object_to_string obj;
            dump_env env;
            raise InvalidProgram
      in
      unwrap proc
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
  | FixIntro (n, expr) ->
      eval_expr (bind_fixes env n) expr
  | FixSet (addr, arg, expr) ->
      let value = eval_value env arg in
      eval_expr (bind_fix env addr value) expr
  | Dbg (value, expr) ->
      let obj = eval_value env value in
      print_endline @@ object_to_string obj;
      eval_expr (bind env obj) expr
  | _ -> raise InvalidProgram

and eval_apply (env : env) (proc : proc) (args : obj list) : obj =
  match (proc, args) with
  | Expr (_, expr), args ->
      (* bind args *)
      let env = List.fold_left bind env args in
      eval_expr env expr
  | Halt, [ arg ] -> (
      match arg with
      | Unit -> exit 0
      | Int i -> exit i
      | _ -> raise InvalidProgram)
  | Cons, args ->
      let args, (env, proc) = args_and_k args in
      eval_apply env proc [ Record args ]
  | Proj, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ Int off; Record fields ] ->
          eval_apply env proc [ List.nth fields off ]
      | _ -> raise InvalidProgram)
  | Not, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ Bool b ] -> eval_apply env proc [ Bool (not b) ]
      | _ -> raise InvalidProgram)
  | And, args ->
      let args, (env, proc) = args_and_k args in
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
      eval_apply env proc [ Bool res ]
  | Or, args ->
      let args, (env, proc) = args_and_k args in
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
      eval_apply env proc [ Bool res ]
  | Xor, args ->
      let args, (env, proc) = args_and_k args in
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
      eval_apply env proc [ Bool res ]
  | Eq, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ a; b ] -> eval_apply env proc [ Bool (a = b) ]
      | _ -> raise InvalidProgram)
  | Neq, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ a; b ] -> eval_apply env proc [ Bool (a <> b) ]
      | _ -> raise InvalidProgram)
  | Gt, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ Int a; Int b ] -> eval_apply env proc [ Bool (a > b) ]
      | [ Real a; Real b ] -> eval_apply env proc [ Bool (a > b) ]
      | _ -> raise InvalidProgram)
  | Gte, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ Int a; Int b ] -> eval_apply env proc [ Bool (a >= b) ]
      | [ Real a; Real b ] -> eval_apply env proc [ Bool (a >= b) ]
      | _ -> raise InvalidProgram)
  | Lt, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ Int a; Int b ] -> eval_apply env proc [ Bool (a < b) ]
      | [ Real a; Real b ] -> eval_apply env proc [ Bool (a < b) ]
      | _ -> raise InvalidProgram)
  | Lte, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ Int a; Int b ] -> eval_apply env proc [ Bool (a <= b) ]
      | [ Real a; Real b ] -> eval_apply env proc [ Bool (a <= b) ]
      | _ -> raise InvalidProgram)
  | Add, args ->
      let args, (env, proc) = args_and_k args in
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
      eval_apply env proc [ res ]
  | Sub, args ->
      let args, (env, proc) = args_and_k args in
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
      eval_apply env proc [ res ]
  | Mul, args ->
      let args, (env, proc) = args_and_k args in
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
      eval_apply env proc [ res ]
  | Div, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ Int a; Int b ] -> eval_apply env proc [ Int (a / b) ]
      | [ Real a; Real b ] -> eval_apply env proc [ Real (Float.div a b) ]
      | _ -> raise InvalidProgram)
  | Mod, args -> (
      let args, (env, proc) = args_and_k args in
      match args with
      | [ Int a; Int b ] -> eval_apply env proc [ Int (a mod b) ]
      | [ Real a; Real b ] -> eval_apply env proc [ Real (Float.rem a b) ]
      | _ -> raise InvalidProgram)
  | _ -> raise InvalidProgram

and args_and_k (objs : obj list) : obj list * (env * proc) =
  match objs with
  | [] -> raise InvalidProgram
  | [ k ] -> (
      match k with
      | Closure (env, proc) -> ([], (env, proc))
      | _ -> raise InvalidProgram)
  | arg :: objs ->
      let args, k = args_and_k objs in
      (arg :: args, k)

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
