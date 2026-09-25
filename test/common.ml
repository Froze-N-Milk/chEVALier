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
