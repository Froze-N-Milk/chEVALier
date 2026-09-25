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

val expr_to_string : expression -> string
val exprs_to_string : expression list -> string

module type S = sig
  type args

  val parse : args -> (expression list, string) result
end

module File : S with type args = Parser.File.args
module String : S with type args = Parser.String.args
