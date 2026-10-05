# chEVALier

chEVAlier is a lispish pure functional programming language.

[video walkthrough](https://youtu.be/Wr0nr00k_jo)

Once installed, you can invoke it on a .EVAL file, which follows the following
grammar:

```
symbol ::=
  ascii word with no whitespace,
  and cannot contain '(', ')', '[', ']', '{', '}', '"', or ';',
  which are the reserved terminal characters

separator ::= any ascii whitespace character

expression ::=
  | <symbol>
  | <string>
  | <round-expression>
  | <square-expression>
  | <curly-expression>

string ::=
  [ <symbol> ] '"' any utf8 encoded text '"'

expression-body ::=
  [ [ <separator> ]
    <expression>
    { <separator> <expression> }*
    [ <separator> ] ]

round-expression ::=
  [ <symbol> ] '(' <expression-body> ')'

square-expression ::=
  [ <symbol> ] '[' <expression-body> ']'

curly-expression ::=
  [ <symbol> ] '{' <expression-body> '}'
```

In addition to the above grammar, ';' converts the rest of the line into a
comment, which is ignored by the parser.

The [demo](./demo.EVAL) is a good example of a simple program.

The language supports the following functions and forms:

## Module

The file as a whole is a `module` which acts like `[define]`, and must have a no
arguments function `main`, that returns either `int` or `unit`. When you run the
file, it will evaluate the `main` function, and then exit with the returned
`int`, or `0` for `unit`.

## Function

The `[fn]` form introduces a function. Specify a list of argument symbols, and
then a body to evaluate when the function is applied.

```
[fn a b c (+ a b c)]
```

This function has arguments `a`, `b`, and `c`, and evaluates `(+ a b c)` when
applied.

Functions may have no arguments.

## Debug

The `?[]` form allows you to debug a value as the program runs.

`?[(% 10 2)]`

When this expression is evaluated, it will print out `DEBUG (% 10 2) => [ 0 ]`.

Debug is the only way to interact with the operating system.

## Let

The `[let]` form allows you to bind symbols to values or functions in order.

```
[let
 [x 10]
 [y (/ x 2)]
 [(z a) (+ x y a)]
 (z 1)]
```

This `[let]` binds `x` to `10`, `y` to `(/ x 2)`, and `z` to `[fn a (+ x y a)]`
then evaluates `(z 1)`.

Note that functions have a special binding form to make them easier to bind.

## Define

The `[define]` form is the same as `[let]`, but allows for mutually recursive
values and functions.

## Switch

While there is internal logic for switch, I did not have time to add and debug a
switch / match form.

## Builtins

The symbol `unit` is initially bound to the constant unit, however, is is not
reserved, and nothing stops you from rebinding it.

The `(cons)` function takes at least one argument, and constructs a record from
all of its arguments.

The `(proj)` function takes at an int and a record, and returns the field of the
record at that index, starting from 0.

NOTE: I have not tested `cons` and `proj`.

The symbols `true` and `false` are initially bound to the constants true and
false, however, they are not reserved, and nothing stops you from rebinding
them.

The `(!)` function takes one boolean argument, and returns the boolean negation
of it.

The `(&)` function takes at least one boolean argument, and returns the boolean
and of them all.

The `(|)` function takes at least one boolean argument, and returns the boolean
or of them all.

The `(^)` function takes at least one boolean argument, and returns the boolean
xor of them all.

The equality builtins accept any arguments.

The `(=)` function returns the result of comparing two arguments.

The `(!=)` function returns the negation of `(=)`.

All mathematical builtins accept both integers and reals, but expect all
arguments to be the same type, not a mix.

The `(>)` function takes two arguments and returns `true` if the first is
greater than the second.

The `(>=)` function takes two arguments and returns `true` if the first is
greater than or equal to the second.

The `(<)` function takes two arguments and returns `true` if the first is
less than the second.

The `(<=)` function takes two arguments and returns `true` if the first is
less than or equal to the second.

The `(+)` function takes at least one argument, and returns the sum.

The `(-)` function takes at least one argument.
For one argument, it returns the negation.
For more than one argument it returns the first argument with all following
arguments subtracted from it.

The `(*)` function takes at least one argument and returns the multiplication.

The `(/)` function takes two arguments and returns the first divided by the
second.

The `(%)` function takes two arguments and returns the first mod the second.

## Notes

There are plenty of functions missing from the builtins, String concatenation,
int and float conversion, strint conversions for all data types. Adding all
necessary builtins did seem the most pertinent part of the project to me.

`EVAL` can be invoked with the `-d` flag to enable tracing, which prints out the
function and environment at each evaluation step in the program, allowing you to
trace where something went wrong, or the changes in memory as the program is
evaluated.

`EVAL` can be invoked with the `-c` flag to emit a Continuation Passing Style
lowered IR file, which will be named the same as the input .EVAL file, but with
the .chEVAL extension. This file will contain the lowered program, but is not
read again for evaluation. The lowered program is very hard to read, and you may
find the `-d` flag better for seeing how it is evaluated.

# Parsing

I wrote a scannerless monadic parser combinator library in order to facilitate
the parsing. The parsing is the best implemented part of the library, using
OCaml's module functor feaure in order to make the parsing abstract over
different types of parsing inputs. I added support for parsing both strings and
files, and ensured that a good error message will be displayed for invalid
parsing. You can test the error message by mismatching your brackets.

Monadic parser combinators are very nice, as they are a type of recursive
descent parser, which abstract over the effect of consuming a character and back
tracking, allowing the parsing logic to avoid writing out the input state that
is passed between each function call. I also used OCaml's cheap exception system
to enable simple failure, rather than wrap all returned values in an error
type. This also allowed me to keep track of the furthest the parser got into the
input, so I could identify the illegal character and construct a helpful error
message.

Abstracting over the input using a module functor meant that I could support
parsing both strings and files. File IO is not pure in OCaml, while Strings
are pure, and adding my own abstract effect modelling allowed for making both
input types look pure again, with more explicit effect handling.

I did not consult any particular resources on how to do parsing.

# CPS Transform

Inspired by [1] and [2] I wanted to convert my source program to Continuation
Passing Style (CPS). Which is a superset of the popular Static Single Assignment
(SSA) or Administrative Normal Form (A-Normal form). CPS is better suited to
compilation to machine code, or further analysis and optimisation, which I did
not get the time to do in this project.

I followed the 'smart' algorithm described in [2] with a few major
modifications. The 'smart' algorithm described in [2] is a single pass Direct
Style (DS) to CPS transformation that aims to eliminate a lot of administrative
redexes produced by preexisting transformation algorithms, that would then need
to be removed with later optimisation passes.

Direct style is the normal style that we write programs in, with functions that
return and variable introductions.

Continuation Passing Style is a hard to read and write form of programming where
no functions return and instead they tail-call a continuation function that
contains the rest of the program. This style is reasonably well suited to being
optimised and compiled and is popular for pure functional programming languages,
however is is more complex to reason about for humans than direct style.

The 'smart' transformation algorithm outlined in [2] is also very complex. It
relies on constructing 'meta continuations' and passing data back and forth in a
hard to follow manner to delay emitting a concrete program. This property is
what allows it to avoid producing unwanted redexes, but also makes it hard to
work with, and made it hard to extend it to my programming language's
requirements.

Firstly, I extended the language beyond the exceptionally simple
language described in the paper. This was fairly trivial to do, as it still
relied on the same major constructs. In particular, `[let]` was easy to
implement, as it just required rewriting the source expression to a series of
function declarations and calls.

Secondly, I re-functionalised the structures they constructed, so that the would
be meta-continuations, which allowed for me to introduce new constructs for
emitting the `[define]` forms.

# DeBruijn Indices

My final addition was to convert all symbols to numeric DeBruijn indices. This
decision was likely a mistake, as it did not interact well with the already
complex to follow 'smart' CPS transform algorithm.

DeBruijn indices make it easier for computers to reason about programs by
removing the need to handle symbols and binders in complex ways, instead, each
binder (function) adds a new variable 0, and pushes all other bound variables up
by one, so that the index is the address of the binder that introduces the
variable.

This makes it easier to do both alpha and beta reductions on the program, and to
compare functions. After converting (λa.λb.b) and (λx.λy.y) to use DeBruijn
indices (λ.λ.0) and (λ.λ.0), they are clearly equal, and this is easy for a
computer to reason about.

The complexity of both doing a CPS transformation and also renaming all symbols
to DeBruijn indices meant that I vastly overshot the due date of the
assessment.

# What I Learnt

This was really hard! I suppose my biggest lesson would be to pick a less
ambitious project so I don't submit really late.

I found it very very hard to reason about the interactions between different
parts of the transformation. I spent ~95 hours on this project all up over the
last two weeks according to WakaTime. The vast majority of this was spent on the
conversion / transform algorithm. In particular it took me a very long time to
figure out that I needed the ability to shift generated 'concrete' expressions
after they had been emitted in order to appropriately pass them under binders
later. This probably wasted ~20 hours alone.

I'd spend a long time looking at the generated / executed program, seeing what
was incorrect about it, then trying a million little things to try to nudge it
to behave correctly, which was probably the wrong way to go about this! Often
I'd finish a whole day of tweaking feeling like I'd gone 10 steps forward, 20
steps backward.

In order to help me analyse the program I added the ability to spit out the
final converted script as a .chEVAL file, and when those became hard to read I
added started tracing the function calls in the evaluation logic, which made
analysing the evaluation steps much easier to understand. Both of these
features have been left in as I feel they're fairly well done, and do much
illumination on what's going on under the hood.

Trying DeBruijn indices and adding the mutually recursive `[define]` form was a
bad idea, it was often very difficult to tell what was going on and how the
program would unfold because of the strange times when the closures would be
captured.

In the future I'd like to try A-Normal Form, as having worked with CPS I find it
hard to believe it would be particularly ameanable to compiling to a modern ISA.
Although that sounds like a fun challenge in itself.

I'd also like to try different ways of removing names from source code. In
particular I'd like to try encoding all bindings as 'meta' functions, which
would take a 'name' as a parameter and produce the concrete target language
expression with that name. This idea sounds like it would be easier to work with
and make it easier to later fill in those blanks with De Bruijn indices /
levels, allowing for the same advantages without requiring so much shifting of
already emitted concrete target language expressions.

# AI Use

I did not use any AI tools in any part of the making of this programming
language.

# References

[1] A. W. Appel, Compiling with Continuations. Cambridge: Cambridge University
    Press, 1991.

[2] Milo Davis, William Meehan, and Olin Shivers. 2017. No-brainer CPS
    conversion (functional pearl). Proc. ACM Program. Lang. 1, ICFP, Article 23
    (September 2017), 25 pages. https://doi.org/10.1145/3110267
