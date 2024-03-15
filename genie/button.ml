open Ui
open Styles
open Color

let draw_button widget_id _text (styles : box_styles) box cache =
  let open Maths in
  (* Printf.printf "Draw widget %s with pos %d %d (width %d height %d)\n" widget_id box.x box.y box.width box.height; flush stdout; *)
  match Hashtbl.find_opt cache widget_id with
  | Some (Button Hovered) ->
      Draw.draw_rectangle box.x box.y box.width box.height (styles.hovered_color |> RGB.to_vec3)
  | Some (Button Pressed) ->
      Draw.draw_rectangle box.x box.y box.width box.height (styles.pressed_color |> RGB.to_vec3)
  | Some (Button Inactive) ->
      Draw.draw_rectangle box.x box.y box.width box.height (styles.color |> RGB.to_vec3)
  | None -> Draw.draw_rectangle box.x box.y box.width box.height (styles.color |> RGB.to_vec3)

let str_of_interact = function
  | Inactive -> "Inactive"
  | Pressed -> "Pressed"
  | Hovered -> "Hovered"

let handle_button widget_id apply_onclick (mouse_input : Input.mouse_input) _key_input rect cache
    model =
  let open Maths in
  let mouse_xy = { pos = Vec2i.make mouse_input.x mouse_input.y; extents = Vec2i.make 1 1 } in
  (* print_endline (show_rect rect); *)
  let new_state =
    match (intersects mouse_xy rect, mouse_input.left_button) with
    | true, Held | true, Clicked -> Pressed
    | true, _ -> Hovered
    | _ -> Inactive
  in
  Hashtbl.replace cache widget_id (Button new_state);
  (* print_endline ("Mouse left btn: " ^ (Input.string_of_mouse_action mouse_input.left_button)); *)
  (* print_endline (widget_id ^ " State: " ^ str_of_interact new_state); *)
  if intersects mouse_xy rect && mouse_input.left_button = Released then apply_onclick model
  else model

let default_btn_size = Constraints.{ x_axis = Fixed 200; y_axis = Fixed 80 }

let make ?(text = "") ?(styles = default_box) id onclick : 'model ui_node =
  let computed_size = Maths.{ x = 0; y = 0; width = 0; height = 0 } in
  Widget
    {
      id;
      size = default_btn_size;
      computed_size;
      draw = draw_button id text styles;
      handle_interaction = handle_button id onclick;
    }
