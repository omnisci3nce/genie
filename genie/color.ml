open Maths

[@@@warnerror "-unused-value-declaration"]
(* Color palette will keep triggering unused otherwise *)

let fits_in_u8 i = i >= 0 && i <= 255

module RGB = struct
  type t = { r : int; g : int; b : int }

  let make r g b =
    assert (fits_in_u8 r);
    assert (fits_in_u8 g);
    assert (fits_in_u8 b);
    { r; g; b }

  let black = { r = 0; g = 0; b = 0 }
  let black = { r = 0; g = 0; b = 0 }

  let to_vec3 c =
    Vec3f.make (float_of_int c.r /. 255.0) (float_of_int c.g /. 255.0) (float_of_int c.b /. 255.0)

  let from_vec3 (c : Vec3f.t) =
    {
      r = int_of_float (255.0 *. c.x);
      g = int_of_float (255.0 *. c.y);
      b = int_of_float (255.0 *. c.z);
    }

  let to_floats c = c |> to_vec3 |> Vec3f.scalar_mul 255.0 |> Vec3f.to_tuple
end

let background = Vec3f.make 0.102 0.102 0.114
let foreground = Vec3f.make 0.902 0.886 0.941

(* TailwindCSS Zinc *)
let stone950 = Vec3f.make 0.051 0.039 0.035 |> RGB.from_vec3
let stone900 = Vec3f.make 0.106 0.098 0.090 |> RGB.from_vec3
let stone800 = Vec3f.make 0.161 0.149 0.141 |> RGB.from_vec3
let stone700 = Vec3f.make 0.271 0.251 0.235 |> RGB.from_vec3
let stone600 = Vec3f.make 0.345 0.325 0.306 |> RGB.from_vec3
let stone500 = Vec3f.make 0.471 0.443 0.424 |> RGB.from_vec3
let stone400 = Vec3f.make 0.659 0.635 0.620 |> RGB.from_vec3
let stone300 = Vec3f.make 0.839 0.827 0.820 |> RGB.from_vec3
let stone200 = Vec3f.make 0.906 0.898 0.894 |> RGB.from_vec3
let stone100 = Vec3f.make 0.961 0.961 0.961 |> RGB.from_vec3
let stone50 = Vec3f.make 0.980 0.980 0.980 |> RGB.from_vec3
let red50 = Vec3f.make 1.000 0.924 0.924 |> RGB.from_vec3
let red100 = Vec3f.make 1.000 0.859 0.859 |> RGB.from_vec3
let red200 = Vec3f.make 0.992 0.741 0.741 |> RGB.from_vec3
let red300 = Vec3f.make 0.984 0.624 0.624 |> RGB.from_vec3
let red400 = Vec3f.make 0.976 0.506 0.506 |> RGB.from_vec3
let red500 = Vec3f.make 0.969 0.388 0.388 |> RGB.from_vec3
let red600 = Vec3f.make 0.871 0.349 0.349 |> RGB.from_vec3
let red700 = Vec3f.make 0.761 0.306 0.306 |> RGB.from_vec3
let red800 = Vec3f.make 0.650 0.263 0.263 |> RGB.from_vec3
let red900 = Vec3f.make 0.539 0.220 0.220 |> RGB.from_vec3

(* Normal Colors *)
let cyan = Vec3f.make 0.063 0.8 0.835 |> RGB.from_vec3
let green = Vec3f.make 0.133 0.804 0.545
let magenta = Vec3f.make 0.639 0.294 0.69
let red = Vec3f.make 0.878 0.235 0.235
let white = Vec3f.make 0.796 0.796 0.796
let yellow = Vec3f.make 0.933 0.753 0.318
let black = Vec3f.make 0.231 0.231 0.231
let blue = Vec3f.make 0.4 0.545 0.886

(* Bright Colors *)
let bright_black = Vec3f.make 0.4 0.404 0.435
let bright_blue = Vec3f.make 0.506 0.651 0.992
let bright_cyan = Vec3f.make 0.302 0.918 0.918
let bright_green = Vec3f.make 0.247 0.902 0.639
let bright_magenta = Vec3f.make 0.686 0.686 1.0
let bright_red = Vec3f.make 0.941 0.514 0.349
let bright_white = Vec3f.make 1.0 1.0 1.0
let bright_yellow = Vec3f.make 0.984 0.984 0.482
