open ChEVALier.Syntax

let parse parse input =
  (* print input comment *)
  List.iter (fun line -> print_endline @@ "# " ^ line)
  @@ Stdlib.String.split_all input ~sep:"\n";
  (* actually parse *)
  let result = parse input in
  (* print result *)
  (match result with
  | Ok exprs -> print_endline @@ exprs_to_string exprs
  | Error msg -> print_endline @@ msg);
  print_newline ()

module String = struct
  let parse = parse String.parse
  let () = parse "(+ 1 2)"
  let () = parse "[+ 1 2]"
  let () = parse "[+ 1 2)"
  let () = parse "'#hello"
  let () = parse "[ '() ]"
  let () = parse "'()"
  let () = parse "'(+ 1 2)"
  let () = parse "prefix(+ 1 2)"
  let () = parse "'#prefix(+ 1 2)"
  let () = parse "( '#prefix(+ 1 2) '#prefix(+ 1 2) )"
end

module File = struct
  let parse = parse File.parse
  let () = parse "simple.EVAL"
end
