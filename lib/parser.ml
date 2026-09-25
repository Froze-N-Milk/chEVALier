(** defines a monadic parser combinator system for use with {!in_channel} s *)

module type S = sig
  type input
  type 'a t = input -> input * 'a

  exception Mismatch of input

  val dbg : string -> 'a t -> 'a t
  val return : 'a -> 'a t
  val fail : 'a t
  val ( or ) : 'a t -> 'a t -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let*? ) : 'a t -> ('a option -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let+? ) : 'a t -> ('a option -> 'b) -> 'b t
  val first : 'a t list -> 'a t
  val fold_left : ('b -> 'a -> 'a) -> 'a -> 'b t -> 'a t
  val fold_right : ('b -> 'a -> 'a) -> 'a t -> 'b t -> 'a t
  val greedy : 'a t -> 'a Seq.t t
  val consume : _ t -> unit t
  val char : (char -> bool) -> char t
  val eof : unit t
end

module type Input = sig
  type t
  type position

  exception Mismatch of t

  val next : t -> (t * char) option
  val position : t -> position
  val ( or ) : (t -> 'a) -> (t -> 'a) -> t -> 'a
  val to_string : t -> string

  type args

  val parse : args -> (t -> 'a) -> ('a, string) result
end

module Make (Input : Input) = struct
  type input = Input.t
  type position = Input.position
  type 'a t = Input.t -> Input.t * 'a

  exception Mismatch = Input.Mismatch

  let dbg message (p : 'a t) : 'a t =
   fun input ->
    print_endline @@ "entered " ^ message;
    try
      let result = p input in
      print_endline @@ "exited " ^ message ^ " successfully";
      result
    with any ->
      print_endline @@ "exited " ^ message ^ " unsuccessfully";
      raise any

  (** monadic lift *)
  let return a : 'a t = fun input -> (input, a)

  (** failure *)
  let fail (input : Input.t) : Input.t * 'a = raise (Mismatch input)

  (** or *)
  let ( or ) (a : 'a t) (b : 'a t) : 'a t = Input.(a or b)

  (** monadic flatmap *)
  let ( let* ) (a : 'a t) (f : 'a -> 'b t) : 'b t =
   fun input ->
    let input, a = a input in
    f a input

  (** optional monadic flatmap *)
  let ( let*? ) (a : 'a t) (f : 'a option -> 'b t) : 'b t =
    (let* a in
     f @@ Some a)
    or f None

  (** monadic map *)
  let ( let+ ) (a : 'a t) (f : 'a -> 'b) : 'b t =
   fun input ->
    let input, a = a input in
    (input, f a)

  (** optional monadic map *)
  let ( let+? ) (a : 'a t) (f : 'a option -> 'b) : 'b t =
    (let+ a in
     f @@ Some a)
    or fun input -> (input, f None)

  (** tries each in order *)
  let first (parsers : 'a t list) : 'a t = List.fold_right ( or ) parsers fail

  (** left fold recursive combinator *)
  let rec fold_left (f : 'b -> 'a -> 'a) (init : 'a) (p : 'b t) : 'a t =
    let*? x = p in
    match x with Some x -> fold_left f (f x init) p | None -> return init

  (** right fold recursive combinator *)
  let rec fold_right (f : 'b -> 'a -> 'a) (init : 'a t) (p : 'b t) : 'a t =
    let*? x = p in
    match x with
    | Some x ->
        let+ tail = fold_right f init p in
        f x tail
    | None -> init

  (** accumulates a sequence of parsings *)
  let greedy p = fold_right Seq.cons (return Seq.empty) p

  (** as greedy but doesn't store anything *)
  let consume p = fold_left (fun _ _ -> ()) () p

  (** match a character by predicate *)
  let char (pred : char -> bool) =
   fun input ->
    match Input.next input with
    | Some (input, c) when pred c -> (input, c)
    | Some (input, _) -> raise (Mismatch input)
    | _ -> raise (Mismatch input)

  (** match the end of input *)
  let eof =
   fun input ->
    match Input.next input with
    | None -> (input, ())
    | Some (input, _) -> raise (Mismatch input)
end

module File = struct
  type position = int64
  type t = in_channel

  exception Mismatch of t

  let next channel =
    try Some (channel, input_char channel) with End_of_file -> None

  let position channel = LargeFile.pos_in channel

  let ( or ) a b =
   fun channel ->
    let pos = position channel in
    try a channel
    with Mismatch _ ->
      LargeFile.seek_in channel pos;
      b channel

  (* TODO: improve to return line and column number *)
  let to_string channel = "character " ^ Int64.to_string @@ position channel

  type args = string

  let parse file parse =
    try Ok (In_channel.with_open_bin file parse)
    with Mismatch input -> Error (to_string input)
end

module String = struct
  type position = int
  type t = { curr : position; furthest : position; str : string }

  exception Mismatch of t

  let next ({ curr; furthest; str } : t) : (t * char) option =
    if curr < String.length str then
      let next =
        { curr = curr + 1; furthest = max furthest @@ (curr + 1); str }
      in
      let char = String.get str curr in
      Some (next, char)
    else None

  let position ({ curr } : t) = curr

  let ( or ) a b =
   fun input ->
    try a input
    with Mismatch input' ->
      b { input with furthest = max input.furthest input'.furthest }

  (** locates the line and column *)
  let to_string ({ furthest; str } : t) =
    if furthest > String.length str then
      raise @@ Invalid_argument "invalid position";
    let rec f lines line_start pos =
      (* convert eof to new line *)
      let char = if pos < String.length str then String.get str pos else '\n' in
      match char with
      (* eol, found target line *)
      | '\n' when pos >= furthest ->
          let cols = pos - line_start in
          let line = String.sub str line_start cols in
          let line_no = Int.to_string lines in
          "Failed to parse input, encounted unexpected character @ ("
          ^ line_no ^ ":" ^ Int.to_string cols ^ ")\n" ^ line ^ "\n"
          ^ String.make (cols - 1) ' '
          ^ "^"
      (* eol, before target line *)
      | '\n' -> f (lines + 1) (pos + 1) (pos + 1)
      (* otherwise ignore *)
      | _ -> f lines line_start (pos + 1)
    in
    f 1 0 0

  type args = string

  let parse string f =
    try Ok (f { curr = 0; furthest = 0; str = string })
    with Mismatch input -> Error (to_string input)
end
