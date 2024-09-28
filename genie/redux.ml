(* [@@@warning "-26-27-33"]*)

(* open Lens*)
open Utils

type rect = { x : int; y : int; width : int; height : int }

let zero_rect = { x = 0; y = 0; width = 0; height = 0 }

module Spacing = struct
  type t = { left : int; right : int; top : int; bottom : int }

  let global_x = 40
  let global_y = 40
  let zero = { left = 0; right = 0; top = 0; bottom = 0 }
end

type color = Raylib.Color.t

type box_styles = {
  margin : Spacing.t;
  padding : Spacing.t;
  color : color;
  border_width : float;
  border_color : color;
}

type interact = { mouse_x : int; mouse_y : int; mouse_was_released : bool; mouse_is_pressed : bool }
type widget_state = ..
type widget_state += Interactable of [ `Normal | `Hovered | `Pressed ]
type widget_cache = (int, widget_state) Hashtbl.t
type layout_directive = Simple | Row | Column

(** Building blocks of the UI, these are built-in and thus cannot really be extended by the user.
    As you can see they are not parameterised by any user-provided type. *)
type ui_node =
  | Box of {
      id : int;
      computed_rect : rect ref;
      debug_label : string option;
      layout : layout_directive;
      styles : box_styles;
      handle_interact : int -> bool -> interact -> widget_cache -> unit;
      children : ui_node list;
    }
  | Text of { id : int; color : color; contents : string }

(* TODO: More node types: Flex, Vis, Custom (allow you to blit your own pixels) *)

let rec print_ui_tree depth node =
  repeat depth (fun () -> print_char ' ');
  match node with
  | Box { children; _ } ->
      Printf.printf "Box: \n";
      List.iter (print_ui_tree (depth + 2)) children
  | Text { contents; _ } -> Printf.printf "text '%s'\n" contents

let g_id = ref 0

let next_id () =
  incr g_id;
  !g_id

let default_style =
  {
    margin = Spacing.zero;
    padding = Spacing.zero;
    color = Raylib.Color.orange;
    border_width = 0.0;
    border_color = Raylib.Color.black;
  }

let with_margin margin b = { b with margin }
let with_color color b = { b with color }
let with_border_width border_width b = { b with border_width }
let with_border_color border_color b = { b with border_color }

(** Convenience constructor for a Box node *)
let box ?debug_label ?styles ?(interact = fun id hit i c -> ()) ?(layout = Simple) children =
  Box
    {
      id = next_id ();
      debug_label;
      computed_rect = ref zero_rect;
      layout;
      styles = Option.value ~default:default_style styles;
      handle_interact = interact;
      children;
    }

(** Convenience constructor for a Text node *)
let text ?(color = Raylib.Color.black) s = Text { id = next_id (); color; contents = s }

module StableId = struct
  type t = Int of int | Str of string
end

type 'model component = {
  stable_key : StableId.t option;
  view : StableId.t option -> widget_cache -> 'model -> ui_node;
  mount : widget_cache -> unit;
}

let grow_rect (a : rect) (b : rect) : rect =
  let min_x = min a.x b.x in
  let min_y = min a.y b.y in
  let max_x = max (a.x + a.width) (b.x + b.width) in
  let max_y = max (a.y + a.height) (b.y + b.height) in
  { x = min_x; y = min_y; width = max_x - min_x; height = max_y - min_y }

let print_rect r = Printf.printf "X %d Y %d Width %d Height %d\n" r.x r.y r.width r.height

let point_in_rect (x, y) rect =
  x >= rect.x && x <= rect.x + rect.width && y >= rect.y && y <= rect.y + rect.height

let rec get_size (node : ui_node) : rect =
  match node with
  | Box { styles; children; _ } ->
      let width, height =
        List.fold_left
          (fun (w, h) child ->
            let child_size = get_size child in
            (max w child_size.width, h + child_size.height))
          (0, 0) children
      in
      (* The size of a box is the size of its children + its margin *)
      { x = 0; y = 0; width; height = height + styles.margin.top + styles.margin.bottom }
  | Text { contents; _ } ->
      let width = String.length contents * 10 in
      { x = 0; y = 0; width; height = 30 }

let get_margin = function Box { styles = { margin; _ }; _ } -> margin | Text _ -> Spacing.zero
let get_padding = function Box { styles = { padding; _ }; _ } -> padding | Text _ -> Spacing.zero

type render_cmd =
  | R_Rect of rect * Raylib.Color.t
  | R_Outline of rect * Raylib.Color.t
  | R_Text of rect * string * Raylib.Color.t

type renderable = int * render_cmd (* widget id, render cmd *)

let renderables_of_node rect node color =
  match node with
  | Box { styles; _ } ->
      let bg = R_Rect (rect, Option.value color ~default:Raylib.Color.brown) in
      if styles.border_width > 0.0 then
        let border = R_Outline (rect, styles.border_color) in
        [ border; bg ]
      else [ bg ]
  | Text { contents; color; _ } -> [ R_Text (rect, contents, color) ]

let lay_out tree : renderable list =
  (* Scuffed layout algorithm that allocates in the Y axis only *)
  let rec single_pass_layout x y node =
    match node with
    | Text { id; contents; color } ->
        let rect = { x; y; width = String.length contents * 30; height = 30 } in
        renderables_of_node rect node (Some color) |> List.map (fun r -> (id, r))
    | Box { id; layout; styles; children; computed_rect; _ } ->
        let children_size = get_size node in

        (* Start the box after the top margin *)
        let box_x = x + styles.margin.left in
        let box_y = y + styles.margin.top in
        let box_draw_height = children_size.height - styles.margin.top - styles.margin.bottom in
        let box_draw_width = children_size.width - styles.margin.left - styles.margin.right in
        let box_rect = { x = box_x; y = box_y; width = box_draw_width; height = box_draw_height } in
        computed_rect := box_rect;
        let box_renders =
          renderables_of_node box_rect node (Some styles.color) |> List.map (fun r -> (id, r))
        in
        (* Get the size of each one  *)
        let final_x, final_y, children_nodes =
          List.fold_left
            (fun (cx, cy, render_nodes) child ->
              let child_size = get_size child in
              let child_renders = single_pass_layout cx cy child in
              match layout with
              | Simple | Column -> (cx, cy + child_size.height, render_nodes @ child_renders)
              | Row -> (cx + child_size.width, cy, render_nodes @ child_renders))
            (box_x, box_y, []) children
        in
        box_renders @ children_nodes
  in
  single_pass_layout 30 30 tree

let rec interact_tree interaction widget_cache node =
  match node with
  | Box { id; styles; computed_rect; handle_interact; children; _ } ->
      let box_rect = !computed_rect in
      let x, y = (interaction.mouse_x, interaction.mouse_y) in
      let new_state =
        (* hit test *)
        let hit_test = point_in_rect (x, y) box_rect in
        handle_interact id hit_test interaction widget_cache;
        if hit_test then
          if interaction.mouse_is_pressed then Interactable `Pressed else Interactable `Hovered
        else Interactable `Normal
      in
      Hashtbl.replace widget_cache id new_state;
      List.iter (interact_tree interaction widget_cache) children
  | Text _ -> () (* Text doesnt have interactions yet *)

let draw_render_cmd ?(debug = false) font widget_cache (id, node) =
  let color_for_state color = function
    | Interactable `Normal -> color
    | Interactable `Hovered -> Raylib.color_brightness color 0.1
    | Interactable `Pressed -> Raylib.color_brightness color (-0.1)
    | _ -> Raylib.Color.beige
  in
  match node with
  | R_Rect (rect, color) ->
      let color =
        Hashtbl.find_opt widget_cache id |> Option.map (color_for_state color) |> Option.get
      in
      Raylib.draw_rectangle rect.x rect.y rect.width rect.height color;
      if debug then
        Printf.printf "X %d Y %d Width %d Height %d\n" rect.x rect.y rect.width rect.height;
      Raylib.draw_rectangle_lines rect.x rect.y rect.width rect.height Raylib.Color.black
  | R_Outline (rect, color) ->
      Raylib.draw_rectangle_lines rect.x rect.y rect.width rect.height color
  | R_Text (rect, str, color) ->
      Raylib.draw_text_ex font str
        (Raylib.Vector2.create (float_of_int rect.x) (float_of_int rect.y))
        20. 0.0 color

let print_renderable (id, cmd) =
  match cmd with
  | R_Rect (rect, _) -> print_rect rect
  | R_Outline _ -> ()
  | R_Text (rect, str, _) -> Printf.printf "Text x %d y %d %s\n" rect.x rect.y str

let handle_click pos renderables =
  let open Raylib in
  let x, y = (int_of_float (Vector2.x pos), int_of_float (Vector2.y pos)) in
  List.find_opt
    (fun renderable ->
      let rect =
        match snd renderable with
        | R_Rect (r, _) -> r
        | R_Text (r, _, _) -> r
        | R_Outline (r, _) -> r
      in
      point_in_rect (x, y) rect)
    renderables

let rec find_node_by_id target_id node =
  match node with
  | Text { id; _ } -> if id = target_id then Some node else None
  | Box { id; children; _ } ->
      if id = target_id then Some node else List.find_map (find_node_by_id target_id) children

let setup () =
  Raylib.init_window 600 600 "genie - basic window";
  Raylib.set_target_fps 60;
  Raylib.load_font "./Inter-Regular.ttf"

let rec loop font (state : 'model ref) widget_cache (printer : 'model -> unit)
    (builder : 'model ref -> 'model component) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;

    let mouse_pos = Raylib.get_mouse_position () in
    let interaction =
      {
        mouse_x = Raylib.Vector2.x mouse_pos |> int_of_float;
        mouse_y = Raylib.Vector2.y mouse_pos |> int_of_float;
        mouse_is_pressed = Raylib.is_mouse_button_down Raylib.MouseButton.Left;
        mouse_was_released = Raylib.is_mouse_button_released Raylib.MouseButton.Left;
      }
    in

    g_id := 0;

    let tree = (builder state).view None widget_cache !state in

    (* printer !state;*)
    let render_list = lay_out tree in
    interact_tree interaction widget_cache tree;
    render_list |> List.iter (draw_render_cmd ~debug:false font widget_cache);

    if Raylib.is_mouse_button_released Raylib.MouseButton.Left then
      let pos = Raylib.get_mouse_position () in
      match handle_click pos render_list with
      | Some hit_renderable -> (
          let id = fst hit_renderable in
          print_renderable hit_renderable;
          let widget = Option.get (find_node_by_id id tree) in

          match widget with
          | Box { id; handle_interact; _ } -> handle_interact id true interaction widget_cache
          | Text _ -> ())
      | None -> Printf.printf "No hit\n"
    else ();

    end_drawing ();
    loop font state widget_cache printer builder

let demo_window state printer builder =
  let font = setup () in
  let w_cache = Hashtbl.create 10 in

  (builder state).mount w_cache;

  loop font state w_cache printer builder
