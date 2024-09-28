[@@@warning "-26-27-32-33-34-37-69"]

open Genie.Redux
open Raylib.Color

type todo_filter = All | Active | Completed
type ex_model = { todos : (string * bool) list; filter : todo_filter }

let text_builder s = { stable_key = None; mount = (fun _ -> ()); view = (fun _ _ _ -> text s) }

let flex layout (children : 'model component list) : 'model component =
  {
    stable_key = None;
    mount = (fun _ -> ());
    view =
      (fun id c model ->
        (* print_endline "Render Row";*)
        box ~debug_label:"Row" ~layout (List.map (fun child -> child.view None c model) children));
  }

let row = flex Row
let col = flex Column

module SegmentControl = struct
  type t = { tabs : string list; selected_index : int }
  (** Internal widget state *)

  type widget_state += SegmentControl of t

  (** Individual segment tab *)
  let segment idx selected set_index (on_set : 'a -> unit) str =
    {
      stable_key = None;
      mount = (fun _ -> ());
      view =
        (fun id cache _ ->
          box ~debug_label:("Segment " ^ str)
            ~styles:
              (default_style
              |> with_margin { left = 0; right = 0; top = 4; bottom = 4 }
              |> with_color (if selected then white else lightgray))
            ~interact:(fun _ hit int cache ->
              if hit && int.mouse_was_released then (
                set_index idx;
                on_set str))
            [ text str ]);
    }

  (** Creates a Segment Control component *)
  let make (key : StableId.t) (tabs : string list) (setter : 'a -> unit) : 'model component =
    let init_state cache =
      Hashtbl.add cache 88 (SegmentControl { tabs; selected_index = 0 });
      SegmentControl { tabs; selected_index = 0 }
    in
    {
      stable_key = Some key;
      mount = (fun cache -> ignore (init_state cache));
      view =
        (fun _ cache model ->
          let state =
            match Hashtbl.find_opt cache 88 with Some s -> s | None -> init_state cache
          in
          let state =
            match state with
            | SegmentControl s -> s
            | _ -> failwith "Incorrect state type in cache "
          in
          let set_selected_index index =
            Hashtbl.replace cache 88 (SegmentControl { state with selected_index = index })
          in
          box ~layout:Row
            (List.mapi
               (fun index label ->
                 (segment index (index = state.selected_index) set_selected_index
                    (fun _ -> setter label)
                    label)
                   .view
                   None cache model)
               tabs));
    }
end

(* Let's try and develop a text input *)
(* What does it have?
   - box for the background
   - border (rounded)
   - on hover the border or background can change
   - selectable, when selected the border or background color can change
   - when selected, the cursor should be in the box
   - blinking cursor
*)

module Textbox = struct
  type textbox_internal_state = { selected : bool; text : string }

  type widget_state +=
    | Textbox of textbox_internal_state
          (** This is the state that gets stored in the widget_cache *)

  type t

  let backspace s = String.sub s 0 (String.length s - 1)

  (** Interaction handling for [Textbox] *)
  let textbox_interact : int -> bool -> interact -> widget_cache -> unit =
   fun id hit interact cache ->
    let state =
      match Hashtbl.find_opt cache 99 with
      | Some (Textbox s) -> s
      | _ -> { selected = false; text = "  placeholder   " }
    in
    (* Key handling *)
    let state =
      if Raylib.is_key_released Backspace then { state with text = backspace state.text }
      else
        match Genie.Utils.get_typed_key () with
        | Some s -> { state with text = state.text ^ s }
        | None -> state
    in
    Hashtbl.replace cache 99 (Textbox state);
    (* Mouse handling *)
    if hit && interact.mouse_was_released then (
      Printf.printf "ID %d Clicked textbox\n" id;
      flush stdout;

      print_endline ("Set to " ^ string_of_bool (not state.selected));
      Hashtbl.replace cache 99 (Textbox { selected = not state.selected; text = "  " }))
    else ()

  let swap f g a = g (f a)

  let make ?stable_key () : 'model component =
    {
      stable_key;
      mount = (fun _ -> ());
      view =
        (fun id cache _ ->
          let key = Option.get stable_key in
          let state =
            match Hashtbl.find_opt cache 99 with
            | Some (Textbox s) -> s
            | _ -> { selected = false; text = "  placeholder   " }
          in
          box ~debug_label:"Textbox Outer" ~interact:textbox_interact
            ~styles:
              (default_style
              |> with_color (if state.selected then pink else lightgray)
              |> with_margin { left = 0; right = 0; top = 20; bottom = 0 })
            [ text state.text ]);
    }
end

module Checkbox = struct
  type 'model t = {
    checked : bool;
    disabled : bool;
    label : string;
    onclick : 'model -> bool -> unit;
  }

  (** Creates a new checkbox *)
  let make ?stable_key label =
    { stable_key; mount = (fun _ -> ()); view = (fun _id _cache _ -> text label) }
end

let string_of_filter = function All -> "All" | Active -> "Active" | Completed -> "Completed"

let filter_of_string = function
  | "All" -> All
  | "Active" -> Active
  | "Completed" -> Completed
  | _ -> failwith "Unreachable"

let app state =
  let set_filter (new_filter : string) =
    print_endline ("Set filter to " ^ new_filter);
    state := { !state with filter = filter_of_string new_filter }
  in
  let filter_control =
    SegmentControl.make (StableId.Str "FilterControl") [ "All"; "Active"; "Completed" ] set_filter
  in
  let filter_str = string_of_filter !state.filter in
  let chkbox = Checkbox.make ~stable_key:(StableId.Int 66) "visible" in
  let textbox = Textbox.make ~stable_key:(StableId.Int 99) () in
  let todos =
    !state.todos
    |> List.filter_map (fun (todo, completed) ->
           match !state.filter with
           | All -> Some todo
           | Completed when completed -> Some todo
           | Active when not completed -> Some todo
           | _ -> None)
    |> List.map text_builder
  in
  col
    [
      filter_control;
      row [ col todos; col [ chkbox; text_builder ("Filter: " ^ filter_str); textbox ] ];
    ]

let () =
  let init_model =
    { todos = [ ("Do the dishes", true); ("Clean my desk", false) ]; filter = All }
  in
  let state = ref init_model in
  Genie.Redux.demo_window state
    (fun m -> print_endline ("State: " ^ String.concat " " (List.map fst m.todos)))
    app
