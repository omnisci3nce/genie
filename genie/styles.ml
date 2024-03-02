open Color

type lrtb = (int * int * int * int)
let zero_lrtb = (0, 0, 0, 0)

type box_styles = {
  color: RGB.t;
  padding: lrtb;
  margin: lrtb;
  border_width: lrtb;
  border_color: RGB.t;
  corner_radius: float
}

let default_box = {
  color = RGB.black;
  padding = zero_lrtb;
  margin = zero_lrtb;
  border_width = zero_lrtb;
  border_color = RGB.black;
  corner_radius = 0.0;
}