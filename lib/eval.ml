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
  | Closure (_, Expr expr) ->
      String.concat "" @@ Language.expr_to_string expr []
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
      | None -> "!{ ? }")

let rec objects_to_string objs =
  match objs with
  | [] -> []
  | obj :: objs -> object_to_string obj :: objects_to_string objs

let objects_to_string objs =
  let strs = objects_to_string objs in
  String.concat " " @@ [ "["; String.concat "; " strs; "]" ]

let rec dump_env (env : env) (n : int) (tail : string list) =
  match env with
  | [] -> tail
  | obj :: env ->
      dump_env env (n + 1)
        ((Int.to_string n ^ ": " ^ object_to_string obj) :: tail)

let dump_env (env : env) =
  print_endline "env:";
  print_endline @@ String.concat "\n" @@ dump_env env 0 []

let lookup (env : env) (addr : int) : obj =
  match List.nth_opt env addr with
  | Some obj -> obj
  | None ->
      print_endline @@ "attempted to lookup {" ^ Int.to_string addr
      ^ "}, however it was out of bounds";
      dump_env env;
      raise InvalidProgram

let bind (env : env) (obj : obj) : env = obj :: env

let rec bind_fixes (env : env) (n : int) : env =
  if n = 0 then env else bind_fixes (Fix (ref None) :: env) @@ (n - 1)

let bind_fix (env : env) (fix : obj) (obj : obj) : unit =
  match fix with
  | Fix ref -> (
      match !ref with
      | None -> ref := Some obj
      | Some obj' ->
          print_endline @@ "attempted to set fix " ^ object_to_string (Fix ref)
          ^ " to " ^ object_to_string obj ^ ", however it was already set to "
          ^ object_to_string obj';
          dump_env env;
          raise InvalidProgram)
  | _ ->
      print_endline @@ "attempted to set fix " ^ object_to_string fix ^ " to "
      ^ object_to_string obj ^ ".";
      dump_env env;
      raise InvalidProgram

let pop (env : env) : env * obj =
  match env with
  | [] ->
      print_endline @@ "attempted to pop a value off an empty env";
      dump_env env;
      raise InvalidProgram
  | obj :: env -> (env, obj)

let rec unwrap obj =
  match obj with
  | Fix ref -> ( match !ref with Some obj -> obj | None -> Fix ref)
  | obj -> obj

let rec eval_expr (debug : bool) (env : env) (expr : expr) : obj =
  if debug then (
    print_endline @@ String.concat "" @@ Language.expr_to_string expr [];
    dump_env env;
    print_newline ());
  match expr with
  | Halt arg -> (
      match eval_value env arg with
      | Unit -> exit 0
      | Int i -> exit i
      | _ -> raise InvalidProgram)
  | Apply (Procedure proc, args) ->
      eval_apply debug env proc @@ List.map (eval_value env) args
  | Apply (Var addr, args) -> (
      let proc = unwrap @@ lookup env addr in
      match proc with
      | Closure (env', proc) ->
          eval_apply debug env' proc @@ List.map (eval_value env) args
      | obj ->
          print_endline @@ "expected to find a proc {" ^ Int.to_string addr
          ^ "}, found " ^ object_to_string obj;
          dump_env env;
          raise InvalidProgram)
  | Apply (non_fn, args) ->
      let non_fn = eval_value env non_fn in
      let args = List.map (eval_value env) args in
      print_endline @@ "attempted to apply non-procedure "
      ^ object_to_string non_fn ^ " to arguments: [ "
      ^ (String.concat "; " @@ List.map object_to_string args)
      ^ " ]";
      dump_env env;
      raise InvalidProgram
  | Switch (arg, cases, default) -> (
      match (eval_value env arg, cases) with
      | Unit, [ case ] -> eval_expr debug env case
      | Unit, [] -> eval_expr debug env default
      | Bool true, [ case; _ ] -> eval_expr debug env case
      | Bool true, [ case ] -> eval_expr debug env case
      | Bool false, [ _; case ] -> eval_expr debug env case
      | Bool false, [ _ ] -> eval_expr debug env default
      | Bool _, [] -> eval_expr debug env default
      | Int i, cases -> (
          match List.nth_opt cases i with
          | Some case -> eval_expr debug env case
          | None -> eval_expr debug env default)
      | _ -> raise InvalidProgram)
  | FixIntro (n, expr) -> eval_expr debug (bind_fixes env n) expr
  | FixCons (fix, value, expr) ->
      let fix = eval_value env fix in
      bind_fix env fix @@ eval_value env value;
      eval_expr debug env expr
  | Debug (syntax, values, expr) ->
      let objs = List.map (eval_value env) values in
      print_endline @@ "DEBUG: "
      ^ Syntax.expr_to_string syntax
      ^ " => " ^ objects_to_string objs;
      eval_expr debug env expr

and eval_apply (debug : bool) (env : env) (proc : proc) (args : obj list) : obj
    =
  if debug then (
    print_endline @@ String.concat ""
    @@ ("applying: " :: Language.value_to_string (Procedure proc) []);
    print_endline @@ String.concat "" @@ ("to: " :: [ objects_to_string args ]);
    dump_env env;
    print_newline ());
  match (proc, args) with
  | Expr expr, args ->
      (* bind args *)
      let env = List.fold_left bind env args in
      eval_expr debug env expr
  | Halt, args -> (
      match args with
      | [ Unit ] -> exit 0
      | [ Int i ] -> exit i
      | _ ->
          print_endline @@ String.concat ""
          @@ ("invalid arguments: " :: [ objects_to_string args ]);
          dump_env env;
          raise InvalidProgram)
  | Cons, args ->
      let args, (env, proc) = args_and_k env args in
      let env = bind env @@ Record args in
      eval_apply debug env proc []
  | Proj, args -> (
      let args, (env, proc) = args_and_k env args in
      match args with
      | [ Int off; Record fields ] ->
          let env = bind env @@ List.nth fields off in
          eval_apply debug env proc []
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)
  | Not, args -> (
      let args, (env, proc) = args_and_k env args in
      match args with
      | [ Bool b ] -> eval_apply debug env proc [ Bool (not b) ]
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)
  | And, args ->
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      let rec fold res args =
        match args with
        | [] -> res
        | Bool b :: args -> fold (res && b) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let res =
        match args with
        | Bool res :: args -> fold res args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      eval_apply debug env proc [ Bool res ]
  | Or, args ->
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      let rec fold res args =
        match args with
        | [] -> res
        | Bool b :: args -> fold (res || b) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let res =
        match args with
        | Bool res :: args -> fold res args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      eval_apply debug env proc [ Bool res ]
  | Xor, args ->
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      let rec fold res args =
        match args with
        | [] -> res
        | Bool b :: args -> fold (res <> b) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let res =
        match args with
        | Bool res :: args -> fold res args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      eval_apply debug env proc [ Bool res ]
  | Eq, args -> (
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      match args with
      | [ a; b ] -> eval_apply debug env proc [ Bool (a = b) ]
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)
  | Neq, args -> (
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      match args with
      | [ a; b ] -> eval_apply debug env proc [ Bool (a <> b) ]
      | _ -> raise InvalidProgram)
  | Gt, args -> (
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      match args with
      | [ Int a; Int b ] -> eval_apply debug env proc [ Bool (a > b) ]
      | [ Real a; Real b ] -> eval_apply debug env proc [ Bool (a > b) ]
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)
  | Gte, args -> (
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      match args with
      | [ Int a; Int b ] -> eval_apply debug env proc [ Bool (a >= b) ]
      | [ Real a; Real b ] -> eval_apply debug env proc [ Bool (a >= b) ]
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)
  | Lt, args -> (
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      match args with
      | [ Int a; Int b ] -> eval_apply debug env proc [ Bool (a < b) ]
      | [ Real a; Real b ] -> eval_apply debug env proc [ Bool (a < b) ]
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)
  | Lte, args -> (
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      match args with
      | [ Int a; Int b ] -> eval_apply debug env proc [ Bool (a <= b) ]
      | [ Real a; Real b ] -> eval_apply debug env proc [ Bool (a <= b) ]
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)
  | Add, args ->
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      let rec foldi res args' =
        match args' with
        | [] -> res
        | Int i :: args -> foldi (res + i) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let rec foldr res args =
        match args with
        | [] -> res
        | Real r :: args -> foldr (Float.add res r) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let res =
        match args with
        | Int res :: args -> Int (foldi res args)
        | Real res :: args -> Real (foldr res args)
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      eval_apply debug env proc [ res ]
  | Sub, args ->
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      let rec foldi res args =
        match args with
        | [] -> res
        | Int i :: args -> foldi (res - i) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let rec foldr res args =
        match args with
        | [] -> res
        | Real r :: args -> foldr (Float.sub res r) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let res =
        match args with
        | [ Int res ] -> Int (-res)
        | [ Real res ] -> Real (Float.neg res)
        | Int res :: args -> Int (foldi res args)
        | Real res :: args -> Real (foldr res args)
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      eval_apply debug env proc [ res ]
  | Mul, args ->
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      let rec foldi res args =
        match args with
        | [] -> res
        | Int i :: args -> foldi (res * i) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let rec foldr res args =
        match args with
        | [] -> res
        | Real r :: args -> foldr (Float.mul res r) args
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      let res =
        match args with
        | Int res :: args -> Int (foldi res args)
        | Real res :: args -> Real (foldr res args)
        | _ ->
            print_endline @@ "invalid arguments: " ^ objects_to_string args;
            dump_env env;
            raise InvalidProgram
      in
      eval_apply debug env proc [ res ]
  | Div, args -> (
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      match args with
      | [ Int a; Int b ] -> eval_apply debug env proc [ Int (a / b) ]
      | [ Real a; Real b ] -> eval_apply debug env proc [ Real (Float.div a b) ]
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)
  | Mod, args -> (
      let args, (env, proc) = args_and_k env args in
      let args = List.map unwrap args in
      match args with
      | [ Int a; Int b ] -> eval_apply debug env proc [ Int (a mod b) ]
      | [ Real a; Real b ] -> eval_apply debug env proc [ Real (Float.rem a b) ]
      | _ ->
          print_endline @@ "invalid arguments: " ^ objects_to_string args;
          dump_env env;
          raise InvalidProgram)

and args_and_k (env : env) (args : obj list) : obj list * (env * proc) =
  match args with
  | [] ->
      print_endline @@ "expected at least one continuation argument, found none";
      dump_env env;
      raise InvalidProgram
  | k :: args -> (
      match unwrap k with
      | Closure (env, proc) -> (args, (env, proc))
      | k ->
          print_endline @@ "unexpected arguments";
          print_endline @@ "continuation: " ^ object_to_string k;
          print_endline
          @@ String.concat "" ("args: " :: [ objects_to_string args ]);
          dump_env env;
          raise InvalidProgram)

and eval_value (env : env) (value : value) : obj =
  match value with
  | Var addr -> lookup env addr
  | Unbound sym ->
      dump_env env;
      raise @@ UnboundSymbol sym
  | Unit -> Unit
  | Bool b -> Bool b
  | Int i -> Int i
  | Real r -> Real r
  | String s -> String s
  | Procedure proc -> Closure (env, proc)
