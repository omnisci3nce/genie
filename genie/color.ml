open Maths

[@@@warnerror "-unused-value-declaration"] (* Color palette will keep triggering unused otherwise *)

let fits_in_u8 i = i >= 0 && i <= 255

module RGB = struct
  type t = { r: int; g: int; b: int }
  let make r g b =
    assert (fits_in_u8 r);
    assert (fits_in_u8 g);
    assert (fits_in_u8 b);
    { r; g; b}
  let black = { r = 0; g = 0; b = 0 }
  let black = { r = 0; g = 0; b = 0 }
  let to_vec3 c = Vec3f.make (float_of_int c.r) (float_of_int c.g) (float_of_int c.b)
  let to_floats c = c |> to_vec3 |> Vec3f.scalar_div 255.0 |> Vec3f.to_tuple
end


let cyan = Vec3f.make 0.063 0.8 0.835

(* TEMPORARY COLOUR PALETTE
Background: (0.102, 0.102, 0.114)
Foreground: (0.902, 0.886, 0.941)

Normal Colors:

Cyan: (0.063, 0.8, 0.835)
Green: (0.133, 0.804, 0.545)
Magenta: (0.639, 0.294, 0.69)
Red: (0.878, 0.235, 0.235)
White: (0.796, 0.796, 0.796)
Yellow: (0.933, 0.753, 0.318)
Black: (0.231, 0.231, 0.231)
Blue: (0.4, 0.545, 0.886)

Bright Colors:

Black: (0.4, 0.404, 0.435)
Blue: (0.506, 0.651, 0.992)
Cyan: (0.302, 0.918, 0.918)
Green: (0.247, 0.902, 0.639)
Magenta: (0.686, 0.686, 1.0)
Red: (0.941, 0.514, 0.349)
White: (1.0, 1.0, 1.0)
Yellow: (0.984, 0.984, 0.482) *)