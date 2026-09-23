(** this module defines the file syntax, and parses it

    chEVALier is a lisp-like language

    {v
    grammar:
      symbol ::=
        ascii word with no whitespace,
        and cannot contain '(', ')', '[', ']', '"', '#' or ';',
        which are the reserved terminal characters

      separator ::= any ascii whitespace character

      prefixed-symbol ::=
        { <symbol> '#' }* <symbol>

      expression ::=
        | <prefixed-symbol>
        | <string>
        | <round-expression>
        | <square-expression>

      string ::=
        // TODO: escaping and interpolation
        [ <prefixed-symbol> ] '"' any utf8 encoded text '"'

      expression-body ::=
        [ [ <separator> ]
          <expression>
          { <separator> <expression> }*
          [ <separator> ] ]

      round-expression ::=
        [ <prefixed-symbol> ] '(' <expression-body> ')'

      square-expression ::=
        [ <prefixed-symbol> ] '[' <expression-body> ']'
    v}

    in addition to the above grammar, ';' converts the rest of the line into a
    comment, which is ignored by the parser

    TODO: need to add specifications for escape sequences and talk about how
    bools, chars, numbers, are symbols *)

type brackets = Round | Square

and expression =
  | Prefix of string * expression
  | Sym of string
  | String of string
  | Expr of brackets * expression list

open Parser

let sym_common =
  (* matches a single symbol character *)
  let char =
    char_cond (fun char ->
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
  let char = char_cond Char.Ascii.is_white in
  (* at least one char *)
  let* _ = char in
  (* once one, consume as many as possible *)
  consume char

let escaped_char =
  let* () = char_one '\\' in
  (* TODO: actually handle escapes *)
  char_any

let string =
  let char = first [ escaped_char; char_cond (fun char -> char != '"') ] in
  let* () = char_one '"' in
  let* string = greedy char in
  let+ () = char_one '"' in
  String (String.of_seq string)

(* tries to collect as many prefixes as possible *)
let prefixed (exprk : expression parser) =
  let sym_hash =
    let* sym = sym_common in
    let+ () = char_one '#' in
    sym
  in
  foldr (fun sym expr -> Prefix (sym, expr)) exprk sym_hash

let rec expression channel =
  (prefixed @@ first [ sym; string; round_expression; square_expression ])
    channel

and expression_body channel =
  (let expr =
     let* () = separator in
     expression
   in
   let*? _ = separator in
   let* exprs = greedy expr in
   let+? _ = separator in
   exprs)
    channel

and round_expression channel =
  (let* () = char_one '(' in
   let* exprs = expression_body in
   let+ () = char_one ')' in
   Expr (Round, List.of_seq exprs))
    channel

and square_expression channel =
  (let* () = char_one '[' in
   let* exprs = expression_body in
   let+ () = char_one ']' in
   Expr (Square, List.of_seq exprs))
    channel

let file =
  let* () = separator in
  let* expression in
  let* () = separator in
  let+ () = eof in
  expression

