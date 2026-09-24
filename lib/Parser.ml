(** defines a monadic parser combinator system for use with {!in_channel} s *)

exception Mismatch

module type S = sig
  type input
  type 'a t = input -> input * 'a

  val ( or ) : 'a t -> 'a t -> 'a t
  val return : 'a -> 'a t
  val fail : 'a t
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

  val next : t -> (t * char) option
  val position : t -> position
  val ( or ) : (t -> 'a) -> (t -> 'a) -> t -> 'a
end

module Make (Input : Input) = struct
  type input = Input.t
  type position = Input.position
  type 'a t = Input.t -> Input.t * 'a

  (** monadic lift *)
  let return a : 'a t = fun input -> (input, a)

  (** failure *)
  let fail (_ : Input.t) : Input.t * 'a = raise Mismatch

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
  let first (parsers : 'a t list) : 'a t =
    List.fold_left (fun tail head -> head or tail) fail parsers

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
    | _ -> raise Mismatch

  (** match the end of input *)
  let eof =
   fun input ->
    match Input.next input with None -> (input, ()) | _ -> raise Mismatch
end

module File = struct
  type position = int64
  type t = in_channel

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
end

module String = struct
  type position = int
  type t = position * string

  let next ((pos, str) : t) : (t * char) option =
    if pos < String.length str then Some ((pos + 1, str), String.get str pos)
    else None

  let position (pos, _) = pos
  let ( or ) a b = fun input -> try a input with Mismatch -> b input
end
