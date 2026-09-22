type expression =
  | BoundVar of int
  | UnboundVar of string
  | Val of value
  | Assert of ty * expression
  | Switch of expression * expression list
  | Abstract of string * expression
  | Apply of expression * expression

and value =
  | Type of ty
  | Unit
  | Bool of bool
  | Product of value * value
  | Procedure of expression
  | Construction of int * value

and ty =
  | Never
  | Unit
  | Bool
  | Product of ty * ty
  | Procedure of ty * ty
  | Construction of ty list

type val_ctx = value list
type ty_ctx = ty list

exception Unbound of string
exception NonProcedure of value
exception NonSelect of value
exception InvalidCases of value * expression list
exception TypeCheckFailed of expression

let assertProc v =
  match v with Procedure body -> body | _ -> raise @@ NonProcedure v

let rec eval ctx expr =
  match expr with
  | BoundVar i -> List.nth ctx i
  | UnboundVar str -> raise (Unbound str)
  | Val v -> v
  | Assert (_, expr) -> eval ctx expr
  | Switch (expr, cases) ->
      let value = eval ctx expr in
      let case = select value cases in
      eval ctx case
  | Abstract (sym, expr) -> Procedure (bind sym expr)
  | Apply (proc, arg) ->
      let body = assertProc @@ eval ctx proc in
      let arg = eval ctx arg in
      eval (arg :: ctx) body

and select value cases =
  match value with
  | Unit -> (
      match cases with
      | [ case ] -> case
      | _ -> raise @@ InvalidCases (value, cases))
  | Bool b -> (
      match (b, cases) with
      | true, [ case; _ ] -> case
      | false, [ _; case ] -> case
      | _ -> raise @@ InvalidCases (value, cases))
  | Construction (i, _) -> List.nth cases i
  | _ -> raise @@ NonSelect value

and bind sym expr =
  match expr with
  | BoundVar i -> BoundVar (i + 1)
  | UnboundVar sym' when sym == sym' -> BoundVar 0
  | UnboundVar _ -> expr
  | Val _ -> expr
  | Assert (ty, expr) -> Assert (ty, bind sym expr)
  | Switch (expr, cases) -> Switch (bind sym expr, List.map (bind sym) cases)
  | Abstract (sym', _) when sym == sym' -> expr
  | Abstract (sym', expr) -> Abstract (sym', bind sym expr)
  | Apply (proc, arg) -> Apply (bind sym proc, bind sym arg)

and ty_check ctx (ty : ty) (expr : expression) =
  match expr with
  | BoundVar i when List.nth ctx i == ty -> ty
  | BoundVar _ -> raise @@ TypeCheckFailed expr
  | UnboundVar _ -> raise @@ TypeCheckFailed expr
  | Val value -> (
      match (ty, value) with
      | Unit, Unit -> Unit
      | Bool, Bool _ -> Unit
      | Product (a, b), Product (a', b') ->
          let a = ty_check ctx a @@ Val a' in
          let b = ty_check ctx b @@ Val b' in
          Product (a, b)
      | Procedure (arg, ret), Procedure body ->
          let ret = ty_check (arg :: ctx) ret body in
          Procedure (arg, ret)
      | _ -> raise @@ TypeCheckFailed expr)
  | Assert (ty, expr) -> ty_check ctx ty expr
  | Switch (_, _) -> raise @@ TypeCheckFailed expr
  | Abstract (_, _) -> raise @@ TypeCheckFailed expr
  | Apply (_, _) -> raise @@ TypeCheckFailed expr
