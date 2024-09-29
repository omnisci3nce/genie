open Color

module type Renderer = sig
  val draw_rectangle : int -> int -> int -> int -> RGB.t -> unit

  (* val draw_circle : int -> int -> float -> RGB.t -> unit*)
  val draw_text : int -> int -> string -> RGB.t -> unit
end

module RaylibRenderer : Renderer = struct
  let color rgb = Raylib.Color.create (Color.r rgb) (Color.g rgb) (Color.b rgb) 255

  let draw_rectangle x y w h c =
    let raylib_color = color c in
    Raylib.draw_rectangle x y w h raylib_color

  let draw_text x y contents c = Raylib.draw_text contents x y 18 (color c)
end
