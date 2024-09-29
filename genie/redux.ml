[@@@warning "-6-26-27-32-33-39-69"]

open Utils

type rect = { x : int; y : int; width : int; height : int }

let zero_rect = { x = 0; y = 0; width = 0; height = 0 }

type color = Raylib.Color.t
type font_weight = Regular | Bold

module Spacing = struct
  type t = { left : int; right : int; top : int; bottom : int }
  (** Represents margin OR padding *)

  let merge a b =
    {
      left = max a.left b.left;
      right = max a.right b.right;
      top = max a.top b.top;
      bottom = max a.bottom b.bottom;
    }

  let zero = { left = 0; right = 0; top = 0; bottom = 0 }
end

type box_styles = {
  margin : Spacing.t;
  padding : Spacing.t;
  color : color;
  border_width : float;
  border_color : color;
  corner_radius : float;
  hover_color : color option;
  pressed_color : color option;
}

type text_styles = { weight : font_weight }

module Styles = struct
  (* TODO: implement a styles cache to reduce the size of data that each ui node carries around *)
  let default_style =
    {
      margin = Spacing.zero;
      padding = Spacing.zero;
      color = Raylib.Color.white;
      border_width = 0.0;
      border_color = Raylib.Color.black;
      corner_radius = 0.0;
      hover_color = None;
      pressed_color = None;
    }

  let margin margin b = { b with margin }
  let padding padding b = { b with padding }
  let bg_color color b = { b with color }
  let border_width border_width b = { b with border_width }
  let border_color border_color b = { b with border_color }
end

open Styles

module Design = struct
  type space_token = S_Xs | S_Sm | S_Md | S_Lg | S_Xl
  type scale_value = C_1 | C_2 | C_3 | C_4 | C_5 | C_6 | C_7 | C_8 | C_9 | C_10 | C_11 | C_12

  module type Theme = sig
    open Spacing

    val neutral : scale_value -> color
    val hori_space : space_token -> Spacing.t
    val vert_space : space_token -> Spacing.t
  end

  (** Provides the necessary user-provided functions to convert from design tokens to concrete values *)
  module type ThemeBuilder = sig
    val token_to_spacing : space_token -> int
    val neutral_to_color : scale_value -> string
  end

  let hex_to_raylib_color s =
    let color = int_of_string ("0x" ^ s) in
    let red = color lsr 16 and green = (color lsr 8) land 0xFF and blue = color land 0xFF in
    Raylib.Color.create red green blue 255

  (** Functor that generates a Theme module *)
  module MakeTheme (T : ThemeBuilder) : Theme = struct
    include T

    let hori_space tok =
      let m = token_to_spacing tok in
      Spacing.{ left = m; right = m; top = 0; bottom = 0 }

    let vert_space tok =
      let m = token_to_spacing tok in
      Spacing.{ left = 0; right = 0; top = m; bottom = m }

    let neutral scale_value = neutral_to_color scale_value |> hex_to_raylib_color
  end
end

module StableId = struct
  type t = Int of int | Str of string | Temp of int

  let to_str = function
    | Int i -> "I#" ^ string_of_int i
    | Str s -> "S#" ^ s
    | Temp i -> "T#" ^ string_of_int i

  let hash = function Int i -> Hashtbl.hash i | Str s -> Hashtbl.hash s | Temp i -> Hashtbl.hash i

  let equal a b =
    match (a, b) with
    | Int i1, Int i2 -> i1 = i2
    | Str s1, Str s2 -> s1 = s2
    | Temp t1, Temp t2 -> t1 = t2
    | _ -> false
end

type interact = { mouse_x : int; mouse_y : int; mouse_was_released : bool; mouse_is_pressed : bool }
type widget_state = ..
type widget_state += Interactable of [ `Normal | `Hovered | `Pressed ]

module WidgetCache = struct
  module StableIdHashmap = CCHashtbl.Make (StableId)
  include StableIdHashmap

  type t = widget_state StableIdHashmap.t
end

type axis_constraint = Auto | Fixed of int | PercentOfParent of float
type size_constraint = { x_axis : axis_constraint; y_axis : axis_constraint }
type layout_directive = Simple | Row | Column

(** Building blocks of the UI, these are built-in and thus cannot really be extended by the user.
    As you can see they are not parameterised by any user-provided type. *)
type ui_node =
  | Box of {
      id : StableId.t;
      debug_label : string option;
      computed_rect : rect ref;
      size : size_constraint;
      layout : layout_directive;
      styles : box_styles;
      handle_interact : StableId.t -> hit:bool -> interact -> WidgetCache.t -> unit option;
          (** Id -> was node hit? -> interaction data -> widget cache -> Return Some when you want to consume the event, else None *)
      children : ui_node list;
    }
  | Text of { id : StableId.t; color : color; styles : text_styles; contents : string }
(* TODO: More node types: Vis, Image, Custom (allow you to blit your own pixels) *)

(* TODO: Implement a faster cached layout *)
type computed_node = {
  id : StableId.t;
  mutable computed_rect : rect;
  z_index : int;
  children : computed_node list;
}

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
  StableId.Int !g_id

module BaseTheme = struct
  open Design

  let token_to_spacing = function S_Xs -> 4 | S_Sm -> 8 | S_Md -> 16 | S_Lg -> 24 | S_Xl -> 32

  (* Radix UI's Slate color scale *)
  let neutral_to_color = function
    | C_1 -> "fcfcfd"
    | C_2 -> "f9f9fb"
    | C_3 -> "f0f0f3"
    | C_4 -> "e8e8ec"
    | C_5 -> "e0e1e6"
    | C_6 -> "d9d9e0"
    | C_7 -> "cdced6"
    | C_8 -> "b9bbc6"
    | C_9 -> "8b8d98"
    | C_10 -> "80838d"
    | C_11 -> "60646c"
    | C_12 -> "1c2024"
end

module DefaultTheme = Design.MakeTheme (BaseTheme)

(** Convenience constructor for a Box node *)
let box ?debug_label ?styles ?(size = { x_axis = Auto; y_axis = Auto })
    ?(interact = fun id ~hit i c -> None) ?(layout = Simple) children =
  Box
    {
      id = next_id ();
      debug_label;
      computed_rect = ref zero_rect;
      size;
      layout;
      styles = Option.value ~default:default_style styles;
      handle_interact = interact;
      children;
    }

(** Convenience constructor for a Text node *)
let text ?(color = Raylib.Color.black) ?(weight = Regular) s =
  Text { id = next_id (); color; styles = { weight }; contents = s }

type 'model component = {
  stable_key : StableId.t option;
  view : StableId.t option -> (module Design.Theme) -> WidgetCache.t -> 'model -> ui_node;
  mount : WidgetCache.t -> unit;
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
  | Box { styles; children; size; _ } ->
      let children_width, children_height =
        List.fold_left
          (fun (w, h) child ->
            let child_size = get_size child in
            (max w child_size.width, h + child_size.height))
          (0, 0) children
      in
      let width =
        match size.x_axis with
        | Fixed i -> i
        | _ ->
            children_width + styles.margin.left + styles.margin.right + styles.padding.left
            + styles.padding.right
      in
      let height =
        match size.y_axis with
        | Fixed i -> i
        | _ ->
            children_height + styles.margin.top + styles.margin.bottom + styles.padding.top
            + styles.padding.bottom
      in
      { x = 0; y = 0; width; height }
  | Text { contents; _ } ->
      let width = String.length contents * 9 in
      { x = 0; y = 0; width; height = 30 }

let get_margin = function Box { styles = { margin; _ }; _ } -> margin | Text _ -> Spacing.zero
let get_padding = function Box { styles = { padding; _ }; _ } -> padding | Text _ -> Spacing.zero

type render_cmd =
  | R_Rect of rect * Raylib.Color.t
  | R_Outline of rect * Raylib.Color.t
  | R_Text of rect * string * color * font_weight

type renderable = StableId.t * render_cmd (* widget id, render cmd *)

let renderables_of_node rect node color =
  match node with
  | Box { styles; _ } ->
      let bg = R_Rect (rect, Option.value color ~default:Raylib.Color.brown) in
      if styles.border_width > 0.0 then
        let border = R_Outline (rect, styles.border_color) in
        [ bg; border ]
      else [ bg ]
  | Text { contents; color; styles; _ } -> [ R_Text (rect, contents, color, styles.weight) ]

module Layout = struct
  let lay_out tree : renderable list =
    (* Scuffed layout algorithm that allocates in the Y axis only *)
    let rec single_pass_layout x y node =
      match node with
      | Text { id; contents; color; _ } ->
          let rect = { x; y; width = String.length contents * 30; height = 30 } in
          renderables_of_node rect node (Some color) |> List.map (fun r -> (id, r))
      | Box { id; layout; size; styles; children; computed_rect; _ } ->
          let children_size = get_size node in

          (* Start the box after the top margin *)
          let box_x = x + styles.margin.left in
          let box_y = y + styles.margin.top in
          let box_draw_height =
            match size.y_axis with
            | Fixed i -> i
            | _ -> children_size.height - styles.margin.top - styles.margin.bottom
          and box_draw_width =
            match size.x_axis with
            | Fixed i -> i
            | _ -> children_size.width - styles.margin.left - styles.margin.right
          in
          let box_rect =
            { x = box_x; y = box_y; width = box_draw_width; height = box_draw_height }
          in
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
              (box_x + styles.padding.left, box_y + styles.padding.top, [])
              children
          in
          box_renders @ children_nodes
    in
    single_pass_layout 0 0 tree

  let rec pre_order_traversal f node =
    let visited_node = f node in
    (* We call our function on the current node first *)
    match visited_node with
    | Box { id; debug_label; computed_rect; size; layout; styles; handle_interact; children } as b
      ->
        Box
          {
            id;
            debug_label;
            computed_rect;
            size;
            layout;
            styles;
            handle_interact;
            children = List.map (pre_order_traversal f) children;
          }
    | other -> f other

  let rec post_order_traversal f node =
    match node with
    | Box { id; debug_label; computed_rect; size; layout; styles; handle_interact; children } as b
      ->
        let visited_children = List.map (post_order_traversal f) children in
        f
          (Box
             {
               id;
               debug_label;
               computed_rect;
               size;
               layout;
               styles;
               handle_interact;
               children = visited_children;
             })
    | other -> f other

  let apply_fixed = function
    | Box b ->
        let r = b.computed_rect in
        let computed_span = function Fixed i -> Some i | _ -> None in
        let width = CCOption.get_or ~default:!r.width (computed_span b.size.x_axis) in
        let height = CCOption.get_or ~default:!r.height (computed_span b.size.y_axis) in
        r := { !r with width; height };
        (* print_rect !r;*)
        Box b
    | other -> other

  let layout_ui (window_width, window_height) tree =
    (* Step 1: Propagate fixed sizes *)
    let _ = pre_order_traversal apply_fixed tree in

    (* TODO: Step 2 *)
    tree
end

let rec interact_tree interaction widget_cache node =
  match node with
  | Box { id; styles; computed_rect; handle_interact; children; _ } ->
      let box_rect = !computed_rect in
      let x, y = (interaction.mouse_x, interaction.mouse_y) in
      let new_state =
        (* hit test *)
        let hit_test = point_in_rect (x, y) box_rect in
        let _ = handle_interact id ~hit:hit_test interaction widget_cache in
        if hit_test then
          if interaction.mouse_is_pressed then Interactable `Pressed else Interactable `Hovered
        else Interactable `Normal
      in
      WidgetCache.replace widget_cache id new_state;
      List.iter (interact_tree interaction widget_cache) children
  | Text _ -> () (* Text doesnt have interactions yet *)

let draw_render_cmd ?(debug = false) reg_font bold_font widget_cache (id, node) =
  match node with
  | R_Rect (rect, color) ->
      Raylib.draw_rectangle rect.x rect.y rect.width rect.height color;
      if debug then (
        print_rect rect;
        Raylib.draw_rectangle_lines rect.x rect.y rect.width rect.height Raylib.Color.black)
  | R_Outline (rect, color) ->
      let rrect =
        Raylib.Rectangle.create (float_of_int rect.x) (float_of_int rect.y)
          (float_of_int rect.width) (float_of_int rect.height)
      in
      Raylib.draw_rectangle_lines_ex rrect 1.0 color
  | R_Text (rect, str, color, weight) ->
      Raylib.draw_text_ex
        (if weight = Regular then reg_font else bold_font)
        str
        (Raylib.Vector2.create (float_of_int rect.x) (float_of_int rect.y))
        18. 0.0 color

let print_renderable (id, cmd) =
  match cmd with
  | R_Rect (rect, _) ->
      print_string "[Draw] ";
      print_rect rect
  | R_Outline _ -> ()
  | R_Text (rect, str, _, _) -> Printf.printf "Text x %d y %d %s\n" rect.x rect.y str

let handle_click pos renderables =
  let open Raylib in
  let x, y = (int_of_float (Vector2.x pos), int_of_float (Vector2.y pos)) in
  (* TODO: convert this code to use the ui_node tree now that it has computed_rect *)
  List.find_opt
    (fun renderable ->
      let rect =
        match snd renderable with
        | R_Rect (r, _) -> r
        | R_Text (r, _, _, _) -> r
        | R_Outline (r, _) -> r
      in
      point_in_rect (x, y) rect)
    renderables

let rec find_node_by_id target_id node =
  match node with
  | Text { id; _ } -> if id = target_id then Some node else None
  | Box { id; children; _ } ->
      if id = target_id then Some node else List.find_map (find_node_by_id target_id) children

module App = struct
  type genie_app = {
    theme : (module Design.Theme);
    cache : WidgetCache.t;
    regular_font : Raylib.Font.t;
    bold_font : Raylib.Font.t;
  }
  (** Internal state of a Genie application *)

  let setup (width, height) =
    Raylib.init_window width height "genie - basic window";
    Raylib.set_target_fps 60;
    let regular_font = Raylib.load_font_ex "./Inter-Regular.ttf" 36 None in
    let bold_font = Raylib.load_font_ex "./Inter-Bold.ttf" 36 None in
    {
      regular_font;
      bold_font;
      cache = WidgetCache.create 10;
      theme = (module DefaultTheme : Design.Theme);
    }

  let run genie inital_model builder : unit =
    let rec loop genie (state : 'model ref) (printer : 'model -> unit) builder : unit =
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

        (* Reset temp IDs *)
        g_id := 0;

        let tree = (builder genie.theme state).view None genie.theme genie.cache !state in

        let render_list = tree |> Layout.layout_ui (600, 500) |> Layout.lay_out in
        interact_tree interaction genie.cache tree;
        flush stdout;
        render_list
        |> List.iter (draw_render_cmd ~debug:true genie.regular_font genie.bold_font genie.cache);

        if Raylib.is_mouse_button_released Raylib.MouseButton.Left then
          match handle_click mouse_pos render_list with
          | Some hit_renderable -> (
              let id = fst hit_renderable in
              print_renderable hit_renderable;
              let widget = Option.get (find_node_by_id id tree) in

              match widget with
              | Box { id; handle_interact; _ } ->
                  handle_interact id true interaction genie.cache |> ignore
              | Text _ -> ())
          | None -> Printf.printf "No hit\n"
        else ();

        end_drawing ();

        loop genie state printer builder
    in
    (builder genie.theme inital_model).mount genie.cache;

    loop genie inital_model (fun _ -> ()) builder
end

let text_builder ?(color = Raylib.Color.black) ?(weight = Regular) s =
  { stable_key = None; mount = (fun _ -> ()); view = (fun _ _ _ _ -> text ~color ~weight s) }

let box_builder children =
  { stable_key = None; mount = (fun _ -> ()); view = (fun _ _ _ _ -> box children) }

let list_item onlick s =
  {
    stable_key = None;
    mount = (fun _ -> ());
    view =
      (fun _ theme _ _ ->
        let module Theme = (val theme : Design.Theme) in
        box
          ~interact:(fun id ~hit i _ ->
            if hit && i.mouse_was_released then onlick ();
            Some ())
          [ text s ]);
  }

let flex ~styles ~size layout (children : 'model component list) : 'model component =
  {
    stable_key = None;
    mount = (fun _ -> ());
    view =
      (fun id t c model ->
        box ~debug_label:"Flex" ~layout
          (List.map (fun child -> child.view None t c model) children)
          ~size:(Option.value ~default:{ x_axis = Auto; y_axis = Auto } size)
          ~styles:(Option.value ~default:default_style styles));
  }

let row ?styles ?size = flex ~styles ~size Row
let col ?styles ?size = flex ~styles ~size Column
