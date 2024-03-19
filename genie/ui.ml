open Constraints
open Maths

type basic_interactable = Inactive | Hovered | Pressed
type flex_direction = Column | Row
type flex_spacing = { x_axis : int; y_axis : int }

type widget_state =
  | Button of basic_interactable
  | Checkbox of (basic_interactable * bool)
  | Visible of bool

type widget_id = string
type state_cache = (widget_id, widget_state) Hashtbl.t
type interval = { min : int; max : int }

(** A "thing" that can be drawn, or affects the layout of,
    the drawing parameterised over the user's data model *)
type 'model ui_node =
  | Box of {
      id : widget_id;
      size : size_constraint;
      mutable computed_size : simple_rect;
      draw : simple_rect -> state_cache -> unit;
      handle_interaction :
        Input.mouse_input -> Input.key_input -> rect -> state_cache -> 'model -> 'model;
      children : 'model ui_node list;
    }
  | Flex of {
      dir : flex_direction;
      spacing : flex_spacing;
      mutable computed_size : simple_rect;
      children : 'model ui_node list;
    }
  | Visibility of {
      id : widget_id;
      getter : 'model -> bool;
      mutable computed_size : simple_rect;
      child : 'model ui_node;
    }

let make_visibility id getter child =
  let computed_size = Maths.{ x = 0; y = 0; width = 0; height = 0 } in
  Visibility { id; getter; computed_size; child }

let wid = function Box w -> w.id | Flex _ -> "flex" | Visibility _ -> "visibility"

let string_of_ui_node = function
  | Box { id; computed_size; _ } ->
      Format.sprintf "Box %s {%s}" id (rect_of_simple_rect computed_size |> show_rect)
  | Flex _ -> "Flex: "
  | Visibility _ -> "Visibility: "
(* Format.sprintf "Flex: %s" (List.fold_left (fun s child -> s ^ (string_of_ui_node child)) "" children) *)

let print_ui_node = function
  | Box _ as w ->
      Printf.printf "%s\n" (string_of_ui_node w);
      w
  | Flex _ as f ->
      Printf.printf "%s\n" (string_of_ui_node f);
      f
  | _ as todo -> todo

let rec get_size node =
  match node with
  | Box { computed_size; _ } -> rect_of_simple_rect computed_size
  | Flex { children; _ } ->
      List.fold_left (fun acc child -> Maths.grow_rect acc (get_size child)) zero_rect children
  | Visibility { child; _ } -> get_size child

let rec pre_order_traversal f node =
  let visited_node = f node in
  match visited_node with
  | Flex { dir; children; computed_size; spacing } ->
      let visited_children = List.map (fun child -> pre_order_traversal f child) children in
      Flex { dir; spacing; computed_size; children = visited_children }
  | Visibility { id; getter; computed_size; child } ->
      let visited_child = pre_order_traversal f child in
      Visibility { id; getter; computed_size; child = visited_child }
  | n -> f n

let rec post_order_traversal f node =
  match node with
  | Flex { dir; children; spacing; computed_size } ->
      let visited_children = List.map (fun child -> post_order_traversal f child) children in
      f (Flex { dir; spacing; computed_size; children = visited_children })
  | Visibility { id; getter; computed_size; child } ->
      let visited_child = pre_order_traversal f child in
      f (Visibility { id; getter; computed_size; child = visited_child })
  | n -> f n

let rec update_ui mouse keyboard state_cache model tree =
  match tree with
  | Box { handle_interaction; _ } ->
      let new_model = handle_interaction mouse keyboard (get_size tree) state_cache model in
      new_model
  | Flex { children; _ } ->
      List.fold_left (fun acc t -> update_ui mouse keyboard state_cache acc t) model children
  | Visibility { id; getter; child; _ } ->
      let is_visible = getter model in
      Hashtbl.replace state_cache id (Visible is_visible);
      update_ui mouse keyboard state_cache model child

let layout_ui ?(print_ui_tree = false) scr_width scr_height tree =
  (* Step 1: Propagate fixed sizes *)
  let apply_fixed node =
    match node with
    | Box w ->
        let mutable_rect = w.computed_size in
        let computed_span = function Fixed i -> Some i | _ -> None in
        mutable_rect.width <- CCOption.get_or ~default:mutable_rect.x (computed_span w.size.x_axis);
        mutable_rect.height <- CCOption.get_or ~default:mutable_rect.y (computed_span w.size.y_axis);
        node
    | _ -> node
  in

  let set_x value = function
    | Box w -> w.computed_size.x <- value
    | Flex f -> f.computed_size.x <- value
    | Visibility v -> v.computed_size.x <- value
  in
  let set_y value = function
    | Box w -> w.computed_size.y <- value
    | Flex f -> f.computed_size.y <- value
    | Visibility v -> v.computed_size.y <- value
  in
  let get_width = function
    | Box w -> w.computed_size.width
    | Flex f -> f.computed_size.width
    | Visibility v -> v.computed_size.width
  in
  let get_height = function
    | Box w -> w.computed_size.height
    | Flex f -> f.computed_size.height
    | Visibility v -> v.computed_size.height
  in

  let apply_flex_sizes node =
    match node with
    | Flex { computed_size; _ } as f ->
        let total_size = get_size f in
        computed_size.width <- total_size.extents.x;
        computed_size.height <- total_size.extents.y;
        f
    | _ as node -> node
  in
  let rec compute_positions offset_x offset_y node =
    match node with
    | Flex { dir; children; computed_size; spacing } as flex ->
        let space_allocator ((x, y) : int * int) child =
          let _ = compute_positions x y child in
          let new_x = if dir = Row then x + get_width child + spacing.x_axis else x in
          let new_y = if dir = Column then y + get_height child + spacing.y_axis else y in
          (new_x, new_y)
        in

        (* Adjust the position of the Flex container itself. *)
        computed_size.x <- offset_x;
        computed_size.y <- offset_y;

        (* Iterate over children, allocating space and setting positions. *)
        let _ = List.fold_left space_allocator (offset_x, offset_y) children in
        flex
    | Box { children; _ } as w ->
        set_x offset_x w;
        set_y offset_y w;
        let space_allocator ((x, y) : int * int) child =
          let _ = compute_positions (x + 20) (y + 50) child in
          let new_x = x + get_width child in
          (* We only arrange on a row for now inside box/divs *)
          (new_x, y)
        in
        let _ = List.fold_left space_allocator (offset_x, offset_y) children in
        node
    | Visibility { child; computed_size; _ } ->
        computed_size.x <- offset_x;
        computed_size.y <- offset_y;
        let _ = compute_positions offset_x offset_y child in
        child
  in

  (* These mutate the tree directly for performance reasons and so the output is not used. *)
  let _ = pre_order_traversal apply_fixed tree in
  let _ = post_order_traversal apply_flex_sizes tree in
  if print_ui_tree then
    let _ = pre_order_traversal print_ui_node tree in
    flush stdout
  else ();
  let _ = compute_positions scr_height scr_width tree in
  (* let _ = post_order_traversal (apply_flex_col scr_width scr_height) tree in *)
  ()

let rec draw_ui tree state_cache =
  match tree with
  | Box { draw; computed_size; children; _ } ->
      draw computed_size state_cache;
      List.iter (fun child -> draw_ui child state_cache) children
  | Flex { children; _ } -> List.iter (fun child -> draw_ui child state_cache) children
  | Visibility { id; child; _ } ->
      if
        Hashtbl.find_opt state_cache id
        |> CCOption.map (fun state -> match state with Visible v -> v | _ -> false)
        |> CCOption.value ~default:false
      then draw_ui child state_cache
      else ()
