open Djinn_sys

let get_mouse_pos () =
  let x = get_mouse_x () and y = get_mouse_y () in
  Maths.Vec2i.make x y

let box_params x y width height (color : Maths.Vec3f.t) : Djinn_sys.box_params =
  { x; y; width; height; r = color.x; g = color.y; b = color.z }

let text_params x y (_contents : string) (color : Maths.Vec3f.t) : Djinn_sys.text_params =
  { x; y; r = color.x; g = color.y; b = color.z }
