(* [@@@warning "-26-27-33"]*)

open Lens
open Utils

type rect = { x : int; y : int; width : int; height : int }
type widget_cache = unit
type interact = unit

(** Building blocks of the UI, these are built-in and thus cannot really be extended by the user.
    As you can see they are not parameterised by any user-provided type. *)
type ui_node =
  | Box of { id : int; handle_interact : interact -> widget_cache -> unit; children : ui_node list }
  | Text of { id : int; contents : string }

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
  let id = !g_id in
  g_id := id + 1;
  id

(** Convenience constructor for a Box node *)
let box ?styles ?(interact = fun i c -> ()) children =
  Box { id = next_id (); handle_interact = interact; children }

(** Convenience constructor for a Text node *)
let text s = Text { id = next_id (); contents = s }

type ex_model = { name : string; count : int }
(** Example app state *)

type ('model, 'slice) component_builder = 'model ref -> ('model, 'slice) Lens.t -> ui_node
type 'model component = { view : 'model -> ui_node }

(* brainstorming *)
let column_component (children : string list) : unit -> unit -> ui_node =
 fun env parent ->
  Box { id = next_id (); handle_interact = (fun _ _ -> ()); children = List.map text children }

let header_builder model_ref lens =
  let name = lens.get !model_ref in
  text (Format.sprintf "Hello, %s!" name)

let count_display_builder model_ref lens =
  let count = lens.get !model_ref in
  text (Format.sprintf "Count: %d" count)

let increment_btn_builder model_ref lens =
  box
    ~interact:(fun _i _cache ->
      let new_state = lens.set (lens.get !model_ref + 1) !model_ref in
      model_ref := new_state)
    [ text "Click Me" ]

let demo_app () =
  let state = ref { name = "Nathan"; count = 0 } in

  (* Replace this with signals? *)
  let header_lens = { get = (fun m -> m.name); set = (fun _ m -> m) } in
  let count_lens = { get = (fun m -> m.count); set = (fun v m -> { m with count = v }) } in

  let build_tree cur_state =
    (* Header, Count, Increment button *)
    let header = header_builder cur_state header_lens in
    let count = count_display_builder cur_state count_lens in
    let incr_btn = increment_btn_builder cur_state count_lens in
    box [ header; count; incr_btn ]
  in

  (state, build_tree)

let grow_rect (a : rect) (b : rect) : rect =
  let min_x = min a.x b.x in
  let min_y = min a.y b.y in
  let max_x = max (a.x + a.width) (b.x + b.width) in
  let max_y = max (a.y + a.height) (b.y + b.height) in
  { x = min_x; y = min_y; width = max_x - min_x; height = max_y - min_y }

let rec get_size (node : ui_node) : rect =
  match node with
  | Box { children; _ } ->
      List.fold_left
        (fun acc child -> grow_rect acc (get_size child))
        { x = 0; y = 0; width = 0; height = 0 }
        children
  | Text { contents; _ } ->
      let width = String.length contents * 30 in
      { x = 0; y = 0; width; height = 30 }

type render_cmd = R_Rect of rect * Raylib.Color.t | R_Text of rect * string
type renderable = int * render_cmd (* widget id, render cmd *)

let renderable_of_node rect node color =
  match node with
  | Box _ -> R_Rect (rect, Option.value color ~default:Raylib.Color.brown)
  | Text { contents; _ } -> R_Text (rect, contents)

module Padding = struct
  let global_x = 40
  let global_y = 40
  let default_y = 5
end

let lay_out tree : renderable list =
  (* Scuffed layout algorithm that allocates in the Y axis only *)
  let rec single_pass_layout x y node =
    match node with
    | Text { id; contents } ->
        let rect = { x; y; width = String.length contents * 30; height = 30 } in
        [ (id, renderable_of_node rect node None) ]
    | Box { id; children; _ } ->
        let children_size = get_size node in
        let box_render =
          ( id,
            renderable_of_node
              { x; y; width = children_size.width; height = children_size.height }
              node (Some Raylib.Color.orange) )
        in
        (* Get the size of each one and push the y variable down *)
        let final_x, final_y, children_nodes =
          List.fold_left
            (fun (x, y, render_nodes) child ->
              let child_size = get_size node in
              let child_renders = single_pass_layout x y child in
              (x, y + child_size.height, render_nodes @ child_renders))
            (x, y + Padding.default_y, []) (* Extra 5 pixel padding *)
            children
        in
        box_render :: children_nodes
  in
  single_pass_layout Padding.global_x Padding.global_y tree

let draw_render_cmd = function
  | R_Rect (rect, color) -> Raylib.draw_rectangle rect.x rect.y rect.width rect.height color
  | R_Text (rect, str) -> Raylib.draw_text str rect.x rect.y 16 Raylib.Color.black

let print_renderable (id, cmd) =
  match cmd with
  | R_Rect (rect, color) ->
      Printf.printf "Rect x %d y %d c %d\n" rect.x rect.y (Raylib.color_to_int color)
  | R_Text (rect, str) -> Printf.printf "Text x %d y %d %s\n" rect.x rect.y str

let point_in_rect (x, y) rect =
  x >= rect.x && x <= rect.x + rect.width && y >= rect.y && y <= rect.y + rect.height

let handle_click pos renderables =
  let open Raylib in
  let x, y = (int_of_float (Vector2.x pos), int_of_float (Vector2.y pos)) in
  List.find_opt
    (fun renderable ->
      let rect = match snd renderable with R_Rect (r, _) -> r | R_Text (r, _) -> r in
      point_in_rect (x, y) rect)
    renderables

let rec find_node_by_id target_id node =
  match node with
  | Text { id; _ } -> if id = target_id then Some node else None
  | Box { id; children; _ } ->
      if id = target_id then Some node else List.find_map (find_node_by_id target_id) children

let setup () =
  Raylib.init_window 800 600 "genie - basic window";
  Raylib.set_target_fps 60

let rec loop (state : 'model ref) (printer : 'model -> unit)
    (builder : 'model ref -> 'model component) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;

    let tree = (builder state).view !state in

    printer !state;

    let render_list = lay_out tree in
    render_list |> List.map snd |> List.iter draw_render_cmd;

    if Raylib.is_mouse_button_released Raylib.MouseButton.Left then
      let pos = Raylib.get_mouse_position () in
      match handle_click pos render_list with
      | Some hit_renderable -> (
          let id = fst hit_renderable in
          Printf.printf "Clicked on (id%d): " id;
          flush stdout;
          print_renderable hit_renderable;
          let widget = Option.get (find_node_by_id id tree) in

          match widget with Box { handle_interact; _ } -> handle_interact () () | Text _ -> ())
      | None -> Printf.printf "No hit\n"
    else ();

    (* List.iter print_renderable render_list;*)
    end_drawing ();
    loop state printer builder

let demo_window state printer builder =
  setup ();
  loop state printer builder
