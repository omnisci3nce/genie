open Color

type lrtb = int * int * int * int

let zero_lrtb = (0, 0, 0, 0)

type box_styles = {
  color : RGB.t;
  padding : lrtb;
  margin : lrtb;
  border_width : lrtb;
  border_color : RGB.t;
  corner_radius : float;
  hovered_color : RGB.t;
  pressed_color : RGB.t;
}

type text_styles = { color : RGB.t; hovered_color : RGB.t; font_size : float }

let default_box =
  {
    color = RGB.black;
    padding = zero_lrtb;
    margin = zero_lrtb;
    border_width = zero_lrtb;
    border_color = RGB.black;
    corner_radius = 0.0;
    (* Interacted styles *)
    hovered_color = RGB.black;
    pressed_color = RGB.black;
  }

let default_text = { color = RGB.white; hovered_color = RGB.white; font_size = 16.0 }

(* Builder functions *)
let with_color color box : box_styles = { box with color }
let with_hovered_color color box : box_styles = { box with hovered_color = color }
let with_pressed_color color box : box_styles = { box with pressed_color = color }
