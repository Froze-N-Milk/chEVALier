(** defines a monadic parser combinator system for use with {!in_channel} s *)

module type Input = sig
  type t
  type position

  exception Mismatch

  val ( or ) : (t -> 'a) -> (t -> 'a) -> t -> 'a
  val next : t -> (t * char) option
  val position : t -> position
end

module FileInput : Input = struct
  type t = in_channel
  type position = int64

  exception Mismatch

  let next channel =
    try Some (channel, input_char channel) with End_of_file -> None

  let position channel = LargeFile.pos_in channel

  let ( or ) a b =
   fun input ->
    let pos = position input in
    try a input
    with Mismatch ->
      LargeFile.seek_in input pos;
      b input

  let parse file parser = In_channel.with_open_bin file parser
end

module StringInput : Input = struct
  type position = int
  type t = position * string

  exception Mismatch

  let next ((pos, str) : t) : (t * char) option =
    if pos < String.length str then Some ((pos + 1, str), String.get str pos)
    else None

  let position (pos, _) = pos

  let ( or ) a b = fun input -> try a input with Mismatch -> b input
end

module Parser (Input : Input) = struct
  type 'a parser = Input.t -> Input.t * 'a

  (** monadic lift *)
  let return a : 'a parser = fun input -> (input, a)

  (** failure *)
  let fail (_ : Input.t) : Input.t * 'a = raise Input.Mismatch

  (** or *)
  let ( or ) (a : 'a parser) (b : 'a parser) : 'a parser = Input.(a or b)

  (** monadic flatmap *)
  let ( let* ) (a : 'a parser) (f : 'a -> 'b parser) : 'b parser =
   fun input ->
    let input, a = a input in
    f a input

  (** optional monadic flatmap *)
  let ( let*? ) (a : 'a parser) (f : 'a option -> 'b parser) : 'b parser =
    (let* a in
     f @@ Some a)
    or f None

  (** monadic map *)
  let ( let+ ) (a : 'a parser) (f : 'a -> 'b) : 'b parser =
   fun input ->
    let input, a = a input in
    (input, f a)

  (** optional monadic map *)
  let ( let+? ) (a : 'a parser) (f : 'a option -> 'b) : 'b parser =
    (let+ a in
     f @@ Some a)
    or fun input -> (input, f None)

  (** tries each in order *)
  let first (parsers : 'a parser list) : 'a parser =
    List.fold_left (fun tail head -> head or tail) fail parsers

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

  (** match a character by predicate *)
  let char (pred : char -> bool) =
   fun input ->
    match Input.next input with
    | Some (input, c) when pred c -> (input, c)
    | _ -> raise Input.Mismatch

  (** match the end of input *)
  let eof =
   fun input ->
    match Input.next input with
    | None -> (input, ())
    | _ -> raise Input.Mismatch
end
