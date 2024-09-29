open Redux
open Redux.Styles
open Raylib.Color

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
        (fun id theme cache _ ->
          let module Theme = (val theme : Design.Theme) in
          box ~debug_label:("Segment Control Tab" ^ str)
            ~styles:
              (default_style
              |> margin (Theme.vert_space S_Sm)
              |> padding @@ Spacing.merge (Theme.hori_space S_Md) (Theme.vert_space S_Xs)
              |> bg_color (if selected then white else Theme.neutral C_5)
              |> border_width 1.0
              |> border_color (Theme.neutral C_7))
            ~interact:(fun _id ~hit interact cache ->
              if hit && interact.mouse_was_released then (
                set_index idx;
                on_set str;
                Some ())
              else None)
            [ text str ]);
    }

  (** Creates a Segment Control component *)
  let make (key : StableId.t) (tabs : string list) (setter : 'a -> unit) : 'model component =
    {
      stable_key = Some key;
      mount = (fun _ -> ());
      view =
        (fun _ t cache model ->
          let state =
            WidgetCache.get_or_add cache
              ~f:(fun _ -> SegmentControl { tabs; selected_index = 0 })
              ~k:key
          in
          let state =
            match state with
            | SegmentControl s -> s
            | _ -> failwith "Found incorrect widget_state type. Expected SegmentControl"
          in
          let set_selected_index index =
            WidgetCache.replace cache key (SegmentControl { state with selected_index = index })
          in
          box ~layout:Row
            (List.mapi
               (fun index label ->
                 (segment index (index = state.selected_index) set_selected_index
                    (fun _ -> setter label)
                    label)
                   .view
                   None t cache model)
               tabs));
    }
end

module Textbox = struct
  (* Reference material: https://rxi.github.io/textbox_behaviour.html *)

  type textbox_internal_state = { focused : bool; text_buffer : string; selection : int * int }

  let handle_movement ({ selection = head, tail; text_buffer; _ } as s) =
    let length = String.length text_buffer in
    function
    | `DeleteTo p -> s
    | `Left -> { s with selection = (min 0 head - 1, min 0 tail - 1) }
    | `Right -> { s with selection = (max length head + 1, max length tail + 1) }
    | `Start -> s (* TODO *)
    | `End -> s (* TODO *)

  type widget_state +=
    | Textbox of textbox_internal_state
          (** This is the state that gets stored in the widget_cache *)

  type t

  let backspace s = String.sub s 0 (String.length s - 1)

  (** Interaction handling for [Textbox] *)
  let textbox_interact id ~hit interact cache =
    let state =
      match WidgetCache.find_opt cache (StableId.Int 99) with
      | Some (Textbox s) -> s
      | _ -> { focused = false; text_buffer = "  placeholder   "; selection = (0, 0) }
    in
    (* Key handling *)
    let state =
      if state.focused then
        if Raylib.is_key_released Backspace then
          { state with text_buffer = backspace state.text_buffer }
        else
          match Utils.get_typed_key () with
          | Some s -> { state with text_buffer = state.text_buffer ^ s }
          | None -> state
      else state
    in
    WidgetCache.replace cache (StableId.Int 99) (Textbox state);
    (* Mouse handling *)
    if hit && interact.mouse_was_released then (
      Printf.printf "ID %s Clicked textbox\n" (StableId.to_str id);

      print_endline ("Set to " ^ string_of_bool (not state.focused));
      WidgetCache.replace cache (StableId.Int 99)
        (Textbox
           {
             selection = (0, 0);
             focused = not state.focused;
             text_buffer = (if state.focused then state.text_buffer else "  ");
           });
      Some ())
    else None

  let swap f g a = g (f a)

  let make ?stable_key () : 'model component =
    {
      stable_key;
      mount = (fun _ -> ());
      view =
        (fun id _theme cache _ ->
          let _key = Option.get stable_key in
          let state =
            match WidgetCache.find_opt cache (StableId.Int 99) with
            | Some (Textbox s) -> s
            | _ -> { focused = false; text_buffer = "  placeholder   "; selection = (0, 0) }
          in
          box ~debug_label:"Textbox Outer" ~interact:textbox_interact
            ~styles:
              (default_style
              |> bg_color (if state.focused then white else lightgray)
              |> margin { left = 0; right = 0; top = 20; bottom = 0 }
              |> padding { left = 10; right = 10; top = 10; bottom = 0 }
              |> border_width 2.0
              |> border_color (if state.focused then blue else lightgray))
            [ text state.text_buffer ]);
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
    { stable_key; mount = (fun _ -> ()); view = (fun _id _theme _cache _ -> text label) }
end
