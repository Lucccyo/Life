type coords = { x : int; y : int }

module Cell : sig
  type life = Alive | Dead
  type t = { coords : coords; mutable state : life }

  val create : coords -> life -> t
  (*@ t = create c s
      ensures c = t.coords
      ensures s = t.state
  *)

  val switch : t -> unit
  (*@ switch t
      ensures not t.state = old t.state
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
  mutable width : int;
  mutable height : int;
  mutable grid : cells;
  ants : ants;
}

val create : coords list -> coords list -> t
val find : cells -> coords -> Cell.t
val pp : Format.formatter -> t -> unit
