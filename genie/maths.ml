(** Base functions that a 2D Vector must support *)
module type Vector2 = sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val scalar_mul : float -> t -> t
  val dot : t -> t -> float
end

module Vec2Int = struct
  type t = { x : int; y : int }

  let add u v = { x = u.x + v.x; y = u.y + v.y }
  let sub u v = { x = u.x - v.x; y = u.y - v.y }

  let scalar_mul k v =
    let q_k = int_of_float k in
    { x = q_k * v.x; y = q_k * v.y }

  let dot u v = (u.x * v.x) + (u.y * v.y) |> float_of_int
end

module Vec2Float = struct
  type t = { x : float; y : float }

  let add u v = { x = u.x +. v.x; y = u.y +. v.y }
  let sub u v = { x = u.x -. v.x; y = u.y -. v.y }
  let scalar_mul k v = { x = k *. v.x; y = k *. v.y }
  let dot u v = (u.x *. v.x) +. (u.y *. v.y)
end

module Vector2Ops (V : Vector2) = struct
  type t = V.t

  let add = V.add
  let sub = V.sub
  let scalar_mul = V.scalar_mul
  let dot = V.dot
  let neg v = scalar_mul (-1.0) v
  let length v = sqrt (dot v v)
  let magniture = length

  let normalize v =
    let len = length v in
    if len > 0.0 then scalar_mul (1.0 /. len) v else v

  let distance u v = sub u v |> length
end

module Vec2i = Vector2Ops (Vec2Int)
module Vec2f = Vector2Ops (Vec2Float)

type rect = { pos : Vec2i.t; extents : Vec2i.t }

(* TODO: These are not necessary yet*)
type vec3 = { x : float; y : float; z : float }
type mat3x3 = { x_axis : vec3; y_axis : vec3; z_axis : vec3 }
type mat2x2 = { x_axis : Vec2f.t; y_axis : Vec2f.t }
