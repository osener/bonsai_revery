open Core_kernel
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

module Event = struct
  type t = ..
  type t += Ignore | Many of t list

  let sequence e = Many e
  let no_op = Ignore

  module type Handler = Handler
  module type S = S

  let handlers : (t -> unit) Hashtbl.M(Int).t = Hashtbl.create (module Int) ~size:8

  module Define (Handler : Handler) : S with type action := Handler.Action.t and type t := t =
  struct
    type t += C : Handler.Action.t -> t

    let key = Obj.Extension_constructor.id [%extension_constructor C]

    let () =
      Hashtbl.add_exn handlers ~key ~data:(fun inp ->
          match inp with
          | C value -> Handler.handle value
          | _ -> raise_s [%message "Unrecognized variant"])


    let inject v = C v
  end

  let get_key t = Obj.Extension_constructor.id (Obj.Extension_constructor.of_val t)
  let handle_registered_event t = Hashtbl.find_exn handlers (get_key t) t

  module Expert = struct
    let rec handle t =
      match t with
      | Ignore -> ()
      | Many l -> List.iter ~f:handle l
      | t -> handle_registered_event t
  end
end

module Bonsai_lib = Bonsai

module Bonsai = struct
  module Event = Event
  module Incr = Incr
  include Bonsai.Make (Incr) (Event)
end

module Element = struct
  type t = Revery_UI.element
end

module BoundingBox2d = struct
  include Revery_Math.BoundingBox2d

  let get_bounds = getBounds
  let is_point_inside = isPointInside
  let to_string = toString
  let equal = equals
end
