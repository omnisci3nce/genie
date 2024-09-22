[@@@warning "-26-27-33"]

open Lens
open Utils

(*We about to refocus this beeee

  State (signal) DAG

  Render tree of ui_node
  ui_nodes are constructed with a builder

  A 'Component' is a function that returns a UiNode given some props

  show function returns a UiNode and registers a subscriber to the signal to know when to
  rerender ?

  Example scenario:
  State: count: int
  Render:
    box
      header -> text
      text (displays count)
      box -> button -> text
*)

type mouse_input
type keys_input
type widget_cache
type rect = { x : int; y : int; width : int; height : int }
(* type 'a app_context = 'a ref*)
(* type cache = unit (* widget cache and style cache *)*)

type input (* mouse & keys *)
type interact = { input : unit; keys : unit }

(** Building block of the UI, these are built-in and thus cannot really be extended by the user.
    As you can see they are not parameterised by any user-provided type. *)
type ui_node =
  | Box of {
      id : int;
      (* TODO: mutable computed_rect : rect;*)
      handle_interact : interact -> widget_cache -> unit;
      children : ui_node list;
    }
  | Text of { (* mutable computed_rect : rect; *)
              id : int; contents : string }

(* TODO: Flex, Vis, Custom (allow you to draw your own pixels) *)

let rec print_ui_tree depth node =
  repeat depth (fun () -> print_char ' ');
  match node with
  | Box { children; _ } ->
      Printf.printf "Box: \n";
      List.iter (print_ui_tree (depth + 2)) children
  | Text { contents; _ } -> Printf.printf "text '%s'\n" contents

type ('m, 'p) component = Static of ('p -> ui_node) | Dynamic of (('m, 'p) Lens.t -> ui_node)

(* type ('props, 'model, 'slice) component_builder =
     (* input -> cache -> *) 'model ref -> ('model, 'slice) Lens.t option -> 'props component

   let build (builder: ('p, 'm, 's) component_builder) : ui_node =
     let component: 'p component = failwith "TODO" in*)

type header_props = { text : string; size : int }
type count_display_builder = { count : int }
type ex_model = { name : string; count : int }

let app_builder (model : 'a ref) (lens : ('a, 'b) Lens.t) children =
  Box { id = 0; handle_interact = (fun _ _ -> ()); children }

let header_builder model_ref lens =
  let name = lens.get !model_ref in
  Text { id = 1; contents = Format.sprintf "Hello, %s!" name }

let count_display_builder (model : ex_model ref) (lens : (ex_model, int) Lens.t) =
  let count = lens.get !model in
  Text { id = 2; contents = Format.sprintf "Count: %d" count }

let increment_btn_builder model_ref lens =
  Box { id = 3; handle_interact = (fun _i _cache -> ()); children = [] }

let rec handle_interaction (model : 'm) (tree : ui_node) : unit = failwith "TODO"

let demo_app () =
  let state = ref { name = "Nathan"; count = 0 } in

  let header_lens = { get = (fun m -> m.name); set = (fun _ m -> m) } in
  let count_lens = { get = (fun m -> m.count); set = (fun v m -> { m with count = v }) } in

  let build_it cur_state =
    (* Header, Count, Increment button *)
    let header = header_builder cur_state header_lens in
    let count = count_display_builder cur_state count_lens in
    app_builder cur_state Lens.id [ header; count ]
  in

  print_endline "State Before";
  let before = build_it state in
  print_ui_tree 0 before;

  (* update the state using the count lens *)
  state := count_lens.set 1 !state;

  print_endline "State After";
  let after = build_it state in
  print_ui_tree 0 after;
  ()

(* let box (styles : string list) children = Box { handle_interact = (fun i c -> ()); children }*)
(* let text s = Text { contents = s }*)

let frame prev_interact () =
  (* gather input *)
  (* apply interaction *)
  (* update render tree *)
  ()
