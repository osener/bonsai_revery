module Incr = Incr

module type Handler = sig
  module Action : sig
    type t
  end

  val handle : Action.t -> unit
end

module type S = sig
  type action
  type t = private ..
  type t += C : action -> t

  val inject : action -> t
end

module Event : sig
  type t = private ..

  type t +=
    | Ignore (** [Ignore] events are dropped, so no handler is called *)
    | Many of t list
          (** Allows one to represent a list of handlers, which will be individually dispatched to
              their respective handlers. This is so callbacks can return multiple events of whatever
              kind. *)

  val sequence : t list -> t
  val no_op : t

  module type Handler = Handler
  module type S = S

  module Define (Handler : Handler) : S with type action := Handler.Action.t and type t := t

  module Expert : sig
    val handle : t -> unit
  end
end

module Bonsai_lib = Bonsai
module Bonsai : Bonsai.S with module Incr = Incr and module Event = Event

module Element : sig
  type t = UI.element
end

module BoundingBox2d : sig
  type t = Revery_Math.BoundingBox2d.t

  val create : float -> float -> float -> float -> t
  val get_bounds : t -> float * float * float * float
  val intersects : t -> t -> bool
  val intersect : t -> t -> t
  val is_point_inside : x:float -> y:float -> t -> bool
  val transform : t -> Skia.Matrix.t -> t
  val to_string : t -> string
  val equal : t -> t -> bool

  module Mutable = Revery_Math.BoundingBox2d.Mutable
end
