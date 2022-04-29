type coords = { x : int; y : int }

module Cell : sig
  type life = Alive | Dead
  type t = { coords : coords; mutable state : life }

  val create : coords -> life -> t
  (*@ c = create x y s
      ensures x = c.x
      ensures y = c.y
      ensures s = c.state
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
  mutable ants : ants;
}

val create : coords list -> coords list -> t
val find : cells -> coords -> Cell.t
