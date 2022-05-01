type coords = { x : int; y : int }

val add_coords : coords -> coords -> coords
(*@ t = add_coords a b
    ensures t.x = a.x + b.x
    ensures t.y = a.y + b.y
*)

module Cell : sig
  type life = Alive | Dead
  type t = { coords : coords; mutable state : life; mutable prev_state : life }

  val create : coords -> life -> life -> t
  (*@ t = create c s os
      ensures t.coords = c
      ensures t.state = s
      ensures t.prev_state = os
  *)

  val switch : t -> unit
  (*@ switch t
      ensures if t.state = Alive then old t.state = Dead else old t.state = Alive
      ensures t.coords = old t.coords
  *)
end

module Direction : sig
  type t = R | B | L | T

  val cw : t -> t

  (*@ t = cw d
  *)
  val ccw : t -> t
  val to_delta : t -> coords
end

module Ant : sig
  type t = { mutable loc : Cell.t; mutable dir : Direction.t }

  val create : Cell.t -> t
  (*@ t = create c
    ensures t.loc = c
    ensures t.dir = Direction.L
  *)
end

type cells = Cell.t list
type ants = Ant.t list

type t = {
  mutable left : int;
  mutable top : int;
  mutable width : int;
  mutable height : int;
  mutable grid : cells;
  ants : ants;
}
(*champs tj sup a 0*)

val create : coords list -> coords list -> t
(*@ t = create clc cla
    
*)

val grow : t -> unit
val find_cell : cells -> coords -> Cell.t
val pp : Format.formatter -> t -> unit
val to_RLE : t -> int -> unit
