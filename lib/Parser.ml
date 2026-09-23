(** defines a monadic parser combinator system for use with {!in_channel} s *)

type 'a parse_result = 'a option
type 'a parser = in_channel -> 'a parse_result

(** monadic flatmap *)
let ( let* ) (a : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun channel -> Option.bind (a channel) (fun a -> f a channel)

(** optional monadic flatmap *)
let ( let*? ) (a : 'a parser) (f : 'a option -> 'b parser) : 'b parser =
 fun channel ->
  let pos = LargeFile.pos_in channel in
  let a = a channel in
  if Option.is_none a then LargeFile.seek_in channel pos;
  f a channel

(** monadic map *)
let ( let+ ) (a : 'a parser) (f : 'a -> 'b) : 'b parser =
 fun channel -> Option.map f @@ a channel

(** optional monadic map *)
let ( let+? ) (a : 'a parser) (f : 'a option -> 'b) : 'b parser =
 fun channel ->
  let pos = LargeFile.pos_in channel in
  let a = a channel in
  if Option.is_none a then LargeFile.seek_in channel pos;
  Some (f a)

let return a : 'a parser = fun channel -> Some a

(** tries each in order *)
let first (parsers : 'a parser list) : 'a parser =
 fun channel ->
  let pos = LargeFile.pos_in channel in
  let rec first' parsers =
    match parsers with
    | [] -> None
    | parser :: [] -> parser channel
    | parser :: parsers -> (
        match parser channel with
        | None ->
            LargeFile.seek_in channel pos;
            first' parsers
        | ok -> ok)
  in
  first' parsers

(** left fold recursive combinator *)
let rec foldl (f : 'b -> 'a -> 'a) (init : 'a) (p : 'b parser) : 'a parser =
  let*? x = p in
  match x with Some x -> foldl f (f x init) p | None -> return init

(** right fold recursive combinator *)
let rec foldr (f : 'b -> 'a -> 'a) (init : 'a parser) (p : 'b parser) :
    'a parser =
  let*? x = p in
  match x with
  | Some x ->
      let+ tail = foldr f init p in
      f x tail
  | None -> init

(** accumulates a sequence of parsings *)
let greedy p = foldr Seq.cons (return Seq.empty) p

(** as greedy but doesn't store anything *)
let consume p = foldl (fun _ _ -> ()) () p

(** match a single character filtered by predicate [cond] *)
let char_cond cond : char parser =
 fun channel ->
  try
    let char = input_char channel in
    if cond char then Some char else None
  with End_of_file -> None

(** match any single character *)
let char_any : char parser =
 fun channel -> try Some (input_char channel) with End_of_file -> None

(** match a single specified character *)
let char_one char : unit parser =
 fun channel ->
  try
    let char' = input_char channel in
    if char == char' then Some () else None
  with End_of_file -> None

(** match the end of input *)
let eof =
 fun channel ->
  try
    let _ = input_char channel in
    None
  with End_of_file -> Some ()
