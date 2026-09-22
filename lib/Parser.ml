(** this module defines the file syntax, and parses it

    chEVALier is a lisp-like language

    grammar: symbol ::= ascii word with no whitespace, and cannot contain "(",
    ")", "[", "]", "#" or ";", which are the reserved terminal characters

    separator ::= any ascii whitespace character

    prefix ::= [ <symbol> ]

    bool ::= | <prefix> "#t" | <prefix> "#f"

    char ::= <prefix> "#" "'" any utf8 encoded character "'"

    string ::= <prefix> """ any utf8 encoded text """

    expression ::= | [ <symbol> "#" ]<symbol> | <bool> | <char> | <string> |
    <round-expression> | <square-expression>

    expression-body ::= [ <expression> { <separator> <expression> }* ]

    round-expression ::= <prefix> "(" <expression-body> ")" square-expression
    ::= <prefix> "[" <expression-body> "]"

    in addition to the above grammar, ";" converts the rest of the line into a
    comment, which is ignored by the parser

    TODO: need to add specifications for escape sequences and numbers *)

type brackets = Round | Square

type prefix_expression =
  | Prefixed of string * expression
  | Expression of expression

and expression =
  | Sym of string
  | Bool of bool
  | Char of Uchar.t
  | String of string
  | Expr of brackets * prefix_expression list

type 'a parse_result = Ok of 'a | Error
type 'a parser = in_channel -> 'a parse_result

(** monadic flatmap *)
let ( let* ) (a : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun channel -> match a channel with Ok a -> f a channel | _ -> Error

let ( let*$ ) (a, channel) (f : 'a -> 'b parser) : 'b parse_result =
  match a channel with Ok a -> f a channel | _ -> Error

(** monadic map *)
let ( let+ ) (a : 'a parser) (f : 'a -> 'b) : 'b parser =
 fun channel -> match a channel with Ok a -> Ok (f a) | _ -> Error

let ( let+$ ) (a, channel) (f : 'a -> 'b) : 'b parse_result =
  match a channel with Ok a -> Ok (f a) | _ -> Error

let ok a : 'a parser = fun channel -> Ok a

(** try first *)
let first (parsers : 'a parser list) : 'a parser =
 fun channel ->
  let pos = LargeFile.pos_in channel in
  let rec first' parsers =
    match parsers with
    | [] -> Error
    | parser :: [] -> parser channel
    | parser :: parsers -> (
        match parser channel with
        | Error ->
            LargeFile.seek_in channel pos;
            first' parsers
        | ok -> ok)
  in
  first' parsers

let rec p_rec (p : 'a parser -> 'a parser) = fun channel -> p (p_rec p) channel

let cond_char cond : char parser =
 fun channel ->
  try
    let char = input_char channel in
    if cond char then Ok char else Error
  with End_of_file -> Error

let any_char : char parser =
 fun channel -> try Ok (input_char channel) with End_of_file -> Error

let p_char char : char parser =
 fun channel ->
  try
    let char' = input_char channel in
    if char == char' then Ok char else Error
  with End_of_file -> Error

(* matches a single symbol character *)
let sym_char =
  cond_char (fun char ->
      (* round expressions *)
      char != '(' && char != ')'
      (* square expressions *)
      && char != '['
      && char != ']'
      (* prefix separator and comment *)
      && char != '#'
      && char != ';'
      (* whitespace *)
      && (not @@ Char.Ascii.is_white char))

let sym =
  let sym' sym' =
    let* char = sym_char in
    let+ sym = first [ sym'; ok Seq.empty ] in
    Seq.cons char sym
  in
  (* at least one char *)
  let* char = sym_char in
  (* recursive p match *)
  let+ sym = p_rec sym' in
  Sym (String.of_seq @@ Seq.cons char sym)
