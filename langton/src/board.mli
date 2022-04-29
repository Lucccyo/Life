type coords = { x : int; y : int }

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

module Ant : sig
  type t = { mutable loc : Cell.t }

  val create : Cell.t -> t
  val move : t -> Cell.t -> unit
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
