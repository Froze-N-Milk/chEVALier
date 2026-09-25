type brackets = Round | Square

and expression =
  | Prefix of string * expression
  | Sym of string
  | String of string
  | Expr of brackets * expression list

let rec expr_to_string expr =
  match expr with
  | Prefix (prefix, Sym sym) -> prefix ^ "#" ^ sym
  | Prefix (prefix, Prefix (prefix', expr)) ->
      prefix ^ "#" ^ expr_to_string (Prefix (prefix', expr))
  | Prefix (prefix, expr) -> prefix ^ expr_to_string expr
  | Sym sym -> sym
  | String str -> "\"" ^ str ^ "\""
  | Expr (brackets, exprs) -> (
      match brackets with
      | Round -> "(" ^ exprs_to_string exprs ^ ")"
      | Square -> "[" ^ exprs_to_string exprs ^ "]")

and exprs_to_string exprs =
  match exprs with
  | [] -> ""
  | expr :: exprs -> expr_to_string expr ^ exprs_to_string' exprs

and exprs_to_string' exprs =
  match exprs with
  | [] -> ""
  | expr :: exprs -> " " ^ expr_to_string expr ^ exprs_to_string' exprs

module Make (Input : Parser.Input) = struct
  open Parser.Make (Input)

  type 'a parser = 'a t

  let sym_common =
    (* matches a single symbol character *)
    let char =
      char (fun char ->
          (* string *)
          char != '"'
          (* round expressions *)
          && char != '('
          && char != ')'
          (* square expressions *)
          && char != '['
          && char != ']'
          (* prefix separator and comment *)
          && char != '#'
          && char != ';'
          (* whitespace *)
          && (not @@ Char.Ascii.is_white char))
    in
    (* at least one char *)
    let* x = char in
    (* once one, collect as many as possible *)
    let+ xs = greedy char in
    (* collate them into a String *)
    String.of_seq @@ Seq.cons x xs

  let sym =
    let+ sym = sym_common in
    Sym sym

  let separator =
    let comment =
      (* comment character *)
      let* _ = char @@ ( = ) ';' in
      (* consume line *)
      consume @@ char @@ ( != ) '\n'
    in
    let whitespace =
      let+ _ = char Char.Ascii.is_white in
      ()
    in
    (* must be at least one comment or whitespace *)
    let* _ = comment or whitespace in
    consume @@ (comment or whitespace)

  let escaped_char =
    let* _ = char @@ ( = ) '\\' in
    (* TODO: actually handle escapes *)
    char (fun _ -> true)

  (* tries to collect as many prefixes as possible, then parse an expression *)
  let prefixed (exprk : expression parser) =
    let sym_hash =
      let* sym = sym_common in
      let+ _ = char @@ ( = ) '#' in
      sym
    in
    fold_right (fun sym expr -> Prefix (sym, expr)) exprk @@ sym_hash

  (* collects a final prefix, for everything but sym *)
  let prefix expr =
    let*? sym = sym_common in
    let+ expr in
    match sym with Some sym -> Prefix (sym, expr) | None -> expr

  let string =
    prefix
    @@
    let* _ = char @@ ( = ) '"' in
    let* string = greedy (escaped_char or (char @@ ( != ) '"')) in
    let+ _ = char @@ ( = ) '"' in
    String (String.of_seq string)

  let rec expression input =
    (prefixed @@ first [ string; round_expression; square_expression; sym ])
      input

  and expression_body input =
    (let expr =
       let* expression in
       let+? _ = separator in
       expression
     in
     let*? _ = separator in
     greedy expr)
      input

  and round_expression input =
    (prefix
    @@ let* _ = char @@ ( = ) '(' in
       let* exprs = expression_body in
       let+ _ = char @@ ( = ) ')' in
       Expr (Round, List.of_seq exprs))
      input

  and square_expression input =
    (prefix
    @@ let* _ = char @@ ( = ) '[' in
       let* exprs = expression_body in
       let+ _ = char @@ ( = ) ']' in
       Expr (Square, List.of_seq exprs))
      input

  let parse =
    let*? _ = separator in
    let* exprs =
      greedy
      @@
      let* expression in
      let+? _ = separator in
      expression
    in
    let*? _ = separator in
    let+ () = eof in
    List.of_seq exprs
end

module type S = sig
  type args

  val parse : args -> (expression list, string) result
end

module File = struct
  module Syntax = Make (Parser.File)

  type args = Parser.File.args

  (** opens and parses the contents of a file *)
  let parse args =
    Result.map (fun (_, exprs) -> exprs) @@ Parser.File.parse args Syntax.parse
end

module String = struct
  module Syntax = Make (Parser.String)

  type args = Parser.String.args

  (** parses a string *)
  let parse args =
    Result.map (fun (_, exprs) -> exprs)
    @@ Parser.String.parse args Syntax.parse
end
