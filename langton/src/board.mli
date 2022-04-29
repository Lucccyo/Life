type coords = { x : int; y : int }

val add_coords : coords -> coords -> coords

module Cell : sig
  type life = Alive | Dead
  type t = { coords : coords; mutable state : life }

  val create : coords -> life -> t
  (*@ t = create c s
      ensures t.coords = c
      ensures t.state = s
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
  val ccw : t -> t
  val to_delta : t -> coords
end

module Ant : sig
  type t = { mutable loc : Cell.t; mutable dir : Direction.t }

  val create : Cell.t -> t
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

val create : coords list -> coords list -> t
val find_cell : cells -> coords -> Cell.t
val pp : Format.formatter -> t -> unit
