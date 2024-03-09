open Constraints
open Maths

type basic_interactable = Inactive | Hovered | Pressed
type flex_direction = Column | Row
type widget_state = Button of basic_interactable
type widget_id = string
type widget_cache = (widget_id, widget_state) Hashtbl.t

(** A "thing" that can be drawn, or affects the layout of,
    the drawing parameterised over the user's data model *)
type 'model ui_node =
  | Widget of {
      id : widget_id;
      size : size_constraint;
      mutable computed_size : simple_rect ;
      (* calc_assign_size : size_constraint -> widget_cache -> simple_rect -> unit; *)
      draw : simple_rect -> widget_cache -> unit;
      handle_interaction :
        Input.mouse_input -> Input.key_input -> rect -> widget_cache -> 'model -> 'model;
    }
  | Flex of { dir : flex_direction; children : 'model ui_node list }

let rec get_size node = match node with
  | Widget { computed_size; _ } -> let r =  rect_of_simple_rect computed_size in
      Printf.printf "Button rect: %s\n" (Maths.show_rect r); r
  | Flex { children; _ } -> List.fold_left (fun acc child -> Maths.grow_rect acc (get_size child)) zero_rect children

let rec pre_order_traversal f node =
  let visited_node = f node in
  match visited_node with
  | Flex { dir; children } -> let visited_children = List.map (fun child -> pre_order_traversal f child) children in
      Flex { dir; children = visited_children}
  | n -> f n 

let rec update_ui mouse keyboard widget_cache model tree = match tree with
  | Widget { handle_interaction; _ } ->
      let new_model = handle_interaction mouse keyboard (get_size tree) widget_cache model in
      (new_model, widget_cache)
  | Flex { children; _ } ->
      List.fold_left
        (fun acc t -> update_ui mouse keyboard (snd acc) (fst acc) t)
        (model, widget_cache) children

let layout_ui _scr_width _scr_height tree =
  (* Step 1: Propagate fixed sizes *)
  let apply_fixed node = match node with
    | Widget w -> (
      let mutable_rect = w.computed_size in
    let computed_span = function
    | Fixed i -> Some(i)
    | _ -> None
    in 
    mutable_rect.width <- CCOption.get_or ~default:mutable_rect.x (computed_span w.size.x_axis);
    (* Printf.printf "Set widget width -> %d\n" mutable_rect.width; *)
    mutable_rect.height <- CCOption.get_or ~default:mutable_rect.y (computed_span w.size.y_axis);
    (* Printf.printf "Set widget height -> %d\n" mutable_rect.height; *)
    mutable_rect.x <- 200;
    mutable_rect.y <- 200;

    node
    )
    | _ -> node
  in
  let _ = pre_order_traversal apply_fixed tree in (* This mutates directly *)
  ()


  

let rec draw_ui tree widget_cache = match tree with
  | Widget { draw; computed_size;  _ } -> draw computed_size widget_cache
  | Flex { children; _ } -> List.iter (fun child -> draw_ui child widget_cache) children