open ChEVALier
open Parser.Make (Parser.String)

let _ =
  let parser = char @@ ( = ) '#' in
  Parser.String.parse "#" parser

let _ =
  let parser = char @@ ( == ) '#' in
  Parser.String.parse "#" parser


let _ =
  let parser =
    let*? _ = char @@ ( == ) '_' in
    char @@ ( == ) '#'
  in
  Parser.String.parse "#" parser
