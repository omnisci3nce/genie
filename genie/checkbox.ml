open Ui
open Styles
open Color

let draw_checkbox widget_id (styles : box_styles) box cache =
  let open Maths in
  match Hashtbl.find_opt cache widget_id with
  | Some (Checkbox (Hovered, _)) ->
      Draw.draw_rectangle box.x box.y box.width box.height (styles.hovered_color |> RGB.to_vec3)
  | Some (Checkbox (Pressed, _)) ->
      Draw.draw_rectangle box.x box.y box.width box.height (styles.pressed_color |> RGB.to_vec3)
  | Some (Checkbox (Inactive, false)) ->
      Draw.draw_rectangle box.x box.y box.width box.height (styles.color |> RGB.to_vec3)
  | Some (Checkbox (Inactive, true)) ->
      Draw.draw_rectangle box.x box.y box.width box.height (styles.pressed_color |> RGB.to_vec3)
  | None -> Draw.draw_rectangle box.x box.y box.width box.height (styles.color |> RGB.to_vec3)
  | _ -> ()

let handle_checkbox widget_id (lens : ('model, bool) Lens.t) (mouse_input : Input.mouse_input)
    _key_input rect cache model =
  let open Maths in
  let mouse_xy = { pos = Vec2i.make mouse_input.x mouse_input.y; extents = Vec2i.make 1 1 } in
  (* print_endline (show_rect rect); *)
  let new_state =
    match (intersects mouse_xy rect, mouse_input.left_button) with
    | true, Held | true, Clicked -> Pressed
    | true, _ -> Hovered
    | _ -> Inactive
  in
  let old_state, old_checked =
    match
      Hashtbl.find_opt cache widget_id
      |> Option.value ~default:(Checkbox (Inactive, lens.get model))
    with
    | Checkbox (prev, true) -> (prev, true)
    | Checkbox (prev, false) -> (prev, false)
    | _ -> (Inactive, false)
  in
  let new_checked =
    if intersects mouse_xy rect && mouse_input.left_button = Released && old_state == Pressed then
      not old_checked
    else old_checked
  in
  Hashtbl.replace cache widget_id (Checkbox (new_state, new_checked));
  if new_checked != old_checked then lens.set new_checked model else model

let default_checkbox =
  default_box |> with_color stone200 |> with_hovered_color stone300 |> with_pressed_color stone500

let default_checkbox_size = Constraints.{ x_axis = Fixed 50; y_axis = Fixed 50 }

let make ?(styles = default_checkbox) id (lens : ('model, bool) Lens.t) : 'model ui_node =
  let computed_size = Maths.{ x = 0; y = 0; width = 0; height = 0 } in
  Box
    {
      id;
      size = default_checkbox_size;
      computed_size;
      draw = draw_checkbox id styles;
      handle_interaction = handle_checkbox id lens;
      children = [];
    }
