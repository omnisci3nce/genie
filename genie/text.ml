(** Defines a Text component *)

open Ui
open Styles
open Maths

let draw_text text styles box _cache = Draw.draw_text box.x box.y text styles.color
let handle_text _mouse _key _rect _cache model = model

let make (text : string) (styles : text_styles) id =
  let computed_size = Maths.{ x = 0; y = 0; width = 0; height = 0 } in
  Widget
    {
      id;
      size = Constraints.{ x_axis = FromTextContent text; y_axis = FromTextContent text };
      computed_size;
      draw = draw_text text styles;
      handle_interaction = handle_text;
    }
