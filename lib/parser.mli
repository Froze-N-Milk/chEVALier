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

module Make (Input : Input) : S with type input = Input.t
module File : Input with type args = string
module String : Input with type args = string
