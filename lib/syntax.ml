type brackets = Round | Square | Curly

and expression =
  | Sym of string
  | String of string option * string
  | Expr of string option * brackets * expression list

let rec expr_to_string expr tail =
  match expr with
  | Sym sym -> sym :: tail
  | String (prefix, str) -> "\"" :: String.escaped str :: "\"" :: tail
  | Expr (prefix, brackets, exprs) -> (
      match brackets with
      | Round -> "(" :: exprs_to_string exprs (")" :: tail)
      | Square -> "[" :: exprs_to_string exprs ("]" :: tail)
      | Curly -> "{" :: exprs_to_string exprs ("}" :: tail))

and exprs_to_string exprs tail =
  match exprs with
  | [] -> tail
  | expr :: exprs -> expr_to_string expr (exprs_to_string' exprs tail)

and exprs_to_string' exprs tail =
  match exprs with
  | [] -> tail
  | expr :: exprs -> " " :: expr_to_string expr (exprs_to_string' exprs tail)

let expr_to_string expr = String.concat "" @@ expr_to_string expr []
let exprs_to_string exprs = String.concat "" @@ exprs_to_string exprs []

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
          (* curly expressions *)
          && char != '{'
          && char != '}'
          (* comment *)
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

  let string =
    let* _ = char @@ ( = ) '"' in
    let* string = greedy (escaped_char or (char @@ ( != ) '"')) in
    let+ _ = char @@ ( = ) '"' in
    String (None, String.of_seq string)

  let rec expression input =
    (let*? prefix = sym_common in
     let*? suffix =
       first [ string; round_expression; square_expression; curly_expression ]
     in
     match (prefix, suffix) with
     | Some sym, None -> return @@ Sym sym
     | prefix, Some (String (_, str)) -> return @@ String (prefix, str)
     | prefix, Some (Expr (_, brackets, exprs)) ->
         return @@ Expr (prefix, brackets, exprs)
     | _ -> fail)
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
    (let* _ = char @@ ( = ) '(' in
     let* exprs = expression_body in
     let+ _ = char @@ ( = ) ')' in
     Expr (None, Round, List.of_seq exprs))
      input

  and square_expression input =
    (let* _ = char @@ ( = ) '[' in
     let* exprs = expression_body in
     let+ _ = char @@ ( = ) ']' in
     Expr (None, Square, List.of_seq exprs))
      input

  and curly_expression input =
    (let* _ = char @@ ( = ) '{' in
     let* exprs = expression_body in
     let+ _ = char @@ ( = ) '}' in
     Expr (None, Curly, List.of_seq exprs))
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
