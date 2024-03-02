(** Base functions that a Vector must support *)
module type Vector = sig
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

module Vec3Float = struct
  type t = { x : float; y : float; z : float }

  let add u v = { x = u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z }
  let sub u v = { x = u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z }
  let scalar_mul k v = { x = k *. v.x; y = k *. v.y; z = k *. v.z }
  let dot u v = (u.x *. v.x) +. (u.y *. v.y) +. (u.z *. v.z)
end


module VectorOps (V : Vector) = struct
  type t = V.t
  let add = V.add
  let sub = V.sub
  let scalar_mul = V.scalar_mul
  let dot = V.dot
  let scalar_div s v = scalar_mul (1. /. s) v
  let neg v = scalar_mul (-1.0) v
  let length_squared v = dot v v
  let length v = sqrt (dot v v)
  let magnitude = length

  let normalize v =
    let len = length v in
    if len > 0.0 then scalar_mul (1.0 /. len) v else v

  let distance u v = sub u v |> length
end

module Vec2i_ = VectorOps (Vec2Int)
module Vec2f_ = VectorOps (Vec2Float)
module Vec3f_ = VectorOps (Vec3Float)

module Vec2i = struct
  include Vec2i_
  let make x y : t = { x; y}
end
module Vec2f = struct
  include Vec2f_
  let make x y : t = { x; y}
end
module Vec3f = struct
  include Vec3f_
  
  let make x y z : t = { x; y; z }
  let to_tuple (v: t) = (v.x, v.y, v.z)
end

type rect = { pos : Vec2i.t; extents : Vec2i.t }

(* TODO: These are not necessary yet *)
type mat3x3 = { x_axis : Vec3f.t; y_axis : Vec3f.t; z_axis : Vec3f.t }
type mat2x2 = { x_axis : Vec2f.t; y_axis : Vec2f.t }
