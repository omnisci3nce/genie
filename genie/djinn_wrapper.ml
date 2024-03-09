open Djinn

let get_mouse_pos () =
  let x = get_mouse_x ()
  and y = get_mouse_y () in
  Maths.Vec2i.make x y

let box_params x y width height (color : Maths.Vec3f.t) : Djinn.params =
  { x; y; width; height; r = color.x; g = color.y; b = color.z }