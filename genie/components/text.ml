(** Defines a Text component *)

open Genie
open Genie.Ui

let draw_text text styles box _cache = failwith "unimplemented"
(* let open Genie.Maths in
   (* print_endline "Drawing text"; *)
   Draw.draw_text box.x box.y text (styles.color |> RGB.to_vec3)*)

let handle_text _mouse _key _rect _cache model = model

let make (text : string) (styles : Styles.text_styles) id =
  let computed_size = Maths.{ x = 0; y = 0; width = 0; height = 0 } in
  Box
    {
      id;
      size = Constraints.{ x_axis = FromTextContent text; y_axis = FromTextContent text };
      computed_size;
      draw = draw_text text styles;
      handle_interaction = handle_text;
      children = [];
    }
