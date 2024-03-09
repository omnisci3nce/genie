open Ui

let draw_button widget_id _text box cache = let open Maths in
  match Hashtbl.find_opt cache widget_id with
    | Some (Button Hovered) -> Draw.draw_rectangle 200 200 box.width box.height Color.bright_red
    | _ ->  Draw.draw_rectangle 200 200 box.width box.height Color.bright_yellow

let handle_button widget_id apply_onclick (mouse_input: Input.mouse_input) _key_input rect cache model = let open Maths in
  let mouse_xy = {
    pos = Vec2i.make mouse_input.x mouse_input.y; extents = Vec2i.make 1 1
  } in
  let new_state = if intersects mouse_xy rect then Hovered else Inactive in
  Hashtbl.replace cache widget_id (Button new_state);
  if intersects mouse_xy rect && mouse_input.left_button = Released then begin
    apply_onclick model
  end else
  model

let default_btn_size = Constraints.({ x_axis = Fixed 200; y_axis = Fixed 80 })

let make ?(text = "") id onclick : 'model ui_node =
  Widget {
    id = id;
    size = default_btn_size;
    computed_size = Maths.zero_simple_rect;
    (* calc_assign_size = (fun _ _ -> ()); *)
    draw = draw_button "" text;
    handle_interaction = handle_button "" onclick
  }
