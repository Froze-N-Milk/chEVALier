open ChEVALier

let usage = "EVAL <file.EVAL>"
let file = ref None

let anon_fun filename =
  match !file with
  | Some _ -> failwith "chEVALier can currently only handle one file"
  | None ->
      file := Some filename;
      ()

let debug = ref false
let compiled = ref false

let speclist =
  [
    ("-d", Arg.Set debug, "Output call trace debugging information.");
    ("-c", Arg.Set compiled, "Emit a .chEVAL CPS IR file.");
  ]

let () = Arg.parse speclist anon_fun usage

type expression = Syntax.expression

let parse input : expression list =
  (* parse *)
  let result = Syntax.File.parse input in
  (* return result *)
  match result with
  | Ok exprs -> exprs
  | Error msg ->
      print_endline @@ msg;
      print_newline ();
      exit 1

let parsed =
  match !file with
  | Some file -> parse file
  | None -> failwith "chEVALier expects a single file argument"

let converted = ChEVALier.Convert.cps_module parsed

let cps_file =
  match !file with
  | Some file -> String.drop_last 4 file ^ "chEVAL"
  | None -> failwith "chEVALier expects a single file argument"

let () =
  if !compiled then
    Out_channel.with_open_bin cps_file @@ fun channel ->
    let rec write_all strs =
      match strs with
      | [] -> ()
      | str :: strs ->
          Out_channel.output_string channel str;
          write_all strs
    in
    let strs = ChEVALier.Language.expr_to_string converted [] in
    write_all strs

let obj = ChEVALier.Eval.eval_expr (!debug) [] converted
