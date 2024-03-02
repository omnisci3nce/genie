open Ui

let draw_button widget_id _text _box cache =
  match Hashtbl.find_opt cache widget_id with
    | Some (Button Hovered) -> Draw.draw_rectangle 0 0 0 0
    | _ ->  Draw.draw_rectangle 0 0 0 0

let handle_button _widget_id _apply_onclick _mouse_input _key_input _cache model =
  model

let make ?(text = "") name onclick : 'model drawable =
  Widget {
    name = name;
    size = (fun _ -> ());
    draw = draw_button "" text;
    handle_interaction = handle_button "" onclick
  }
