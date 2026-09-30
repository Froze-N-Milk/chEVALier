(*
type ty = Language.ty
type expr = Language.expr
type value = Language.value
type err
type env

exception TypeCheckError of string

let lookup addr (env : env) : ty = failwith "TODO"
let ok env ty : env * ty = (env, ty)
let err () = raise @@ TypeCheckError "failed to type check"
let ( or ) a b c = try a c with TypeCheckError _ -> b c

let rec check_eq (env : env) (expected : ty) (actual : ty) : env * ty =
  match (expected, actual) with
  | expected, Unknown -> ok env expected
  (* TODO: should replace the binding in the env with the checked value *)
  | Var addr, _ ->
      let expected = lookup addr env in
      check_eq env expected actual
  (* TODO: should replace the binding in the env with the checked value *)
  | expected, Var addr ->
      let actual = lookup addr env in
      check_eq env expected actual
  | Unbound _, _ -> err ()
  | _, Never -> ok env expected
  | Never, _ -> err ()
  | Unit, Unit -> ok env expected
  | Bool, Bool -> ok env expected
  | Int, Int -> ok env expected
  | Real, Real -> ok env expected
  | String, String -> ok env expected
  | Product, Product -> ok env expected
  | Function, Function -> ok env expected
  | Constructor i, Constructor i' when i = i' -> ok env expected
  | Construction (cons, args), Construction (cons', args') ->
      (* TODO: need to push each arg to the env? *)
      let env, cons' = check_eq env cons cons' in
      let env, args' =
        List.fold_right
          (fun (arg, arg') (env, args) ->
            let env, arg' = check_eq env arg arg' in
            (env, arg' :: args))
          (List.combine args args') (env, [])
      in
      ok env @@ Construction (cons', args')
  | _ -> err ()

let rec check_expr (env : env) (ty : ty) (expr : expr) : env * ty =
  match expr with
  | Halt arg ->
      let env, _ = (check_value env Unit or check_value env Unit) arg in
      check_eq env ty Never
  | Assert (ty', expr) ->
      let env, ty' = check_expr env ty' expr in
      check_eq env ty ty'
  | Apply (fn, args) ->
      let (fn_ty : ty) =
        Construction
          ( Function,
            List.fold_right (fun _ args -> (Unknown : ty) :: args) args [ ty ]
          )
      in
      (* checks that the function returns the expected type *)
      let env, fn_ty = check_value env fn_ty fn in
      (* check that the arguments match the arguments of fn_ty *)
      let env, args_ty =
        List.fold_right
        (fun arg (env, args) ->  :: args)
        args
        (env, [])
      in
      _
  | _ -> _

and check_value (env : env) (ty : ty) (value : value) : env * ty =
  match (ty, value) with
  | _, Var addr -> check_eq env ty @@ lookup addr env
  | _, Unbound str -> err ()
  | _, Unit -> check_eq env ty Unit
  | _, Bool _ -> check_eq env ty Bool
  | _, Int _ -> check_eq env ty Int
  | _, Real _ -> check_eq env ty Real
  | _, String _ -> check_eq env ty String
  (* TODO: *)
  | Construction (Function, []), Procedure (Expr (args, expr)) -> _
  | Construction (Function, [ Unit; Never ]), Procedure Halt -> ok ty
  | Construction (Function, [ Int; Never ]), Procedure Halt -> ok ty
  | Construction (Function, args), Procedure Cons ->
      let rec pmatch (args : ty list) =
        match args with
        (* no args is not ok *)
        | [] -> err ()
        (* the return type must be a continuation,
           which we'll return back up *)
        | [ Construction (Product, fields) ] -> ([], fields)
        | arg :: args ->
            let args, fields = pmatch args in
            (arg :: args, fields)
      in
      let args, ret = pmatch args in
      let checked =
        List.fold_right
          (fun (arg, ret) args -> check_eq ret arg :: args)
          (List.combine args ret) ret
      in
      Construction (Function, checked)
  | Construction (Function, [ Int; record; ret ]), Procedure Proj ->
      Construction (Function, checked)
  | _ -> _

and infer_expr (env : env) (expr : expr) : env * ty = _
and infer_value (env : env) (value : value) : env * ty = _
*)
