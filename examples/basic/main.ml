(*
   let counter = ref 4

   let build_ui_tree model =
     let topleft_btn =
       Button.make ~text:"Add Row" ~styles:red_button_style "btn1" (fun model ->
           print_endline "You did it! You clicked me!";
           counter := !counter + 1;
           {
             model with
             topleft = not model.topleft;
             right_column_btns = model.right_column_btns @ [ !counter ];
           })
     let test_checkbox = Checkbox.make "chkbox1" model_show_list in
     let test_text = Text.make "Show/Hide List" default_text "textid" in
     Ui.(
       Flex
         {
           dir = Row;
           spacing = { x_axis = 30; y_axis = 0 };
           computed_size = zero_simple_rect ();
           children =
             [
               Flex
                 {
                   dir = Column;
                   spacing = { x_axis = 0; y_axis = 60 };
                   computed_size = zero_simple_rect ();
                   children = [ topleft_btn; bottomleft_btn; test_checkbox; test_text ];
                 };
               Ui.make_visibility "list_visibility" model_show_list.get
                 (Flex
                    {
                      dir = Column;
                      spacing = { x_axis = 0; y_axis = 30 };
                      computed_size = zero_simple_rect ();
                      children = right_col_btns;
                    });
             ];
         })

     *)

[@@@warning "-26-27-32-33-34-69"]

open Genie.Redux

type ex_model = { todos : string list; filter : [ `All | `Active | `Completed ] }
type env = int

let text_builder s = { view = (fun _ -> text s) }

let row children : 'model component =
  {
    view =
      (fun model ->
        (* print_endline "Render Row";*)
        box ~debug_label:"Row" (List.map (fun child -> child.view model) children));
  }

let segment set str =
  {
    view =
      (fun _ ->
        box ~debug_label:("Segment " ^ str)
          ~styles:
            {
              margin = { left = 0; right = 0; top = 10; bottom = 10 };
              padding = Spacing.zero;
              color = Raylib.Color.blue;
            }
          ~interact:(fun int cache -> if int.mouse_was_released then set str)
          [ text str ]);
  }

(** Creates a Segment Control component *)
let segment_control (setter : string -> unit) (tabs : string list) : 'model component =
  (* Renders each Tab in a row *)
  row (List.map (segment setter) tabs)

let filter_from_str = function
  | "All" -> `All
  | "Active" -> `All
  | "Completed" -> `Completed
  | _ -> failwith "unreachable"

let app state =
  let set_filter (new_filter : string) =
    print_endline ("Set filter to " ^ new_filter);
    state := { !state with filter = filter_from_str new_filter }
  in
  let filter_control = segment_control set_filter [ "All"; "Active"; "Completed" ] in
  filter_control

let () =
  let init_model = { todos = [ "Do the dishes"; "Clean my desk" ]; filter = `All } in
  let state = ref init_model in
  Genie.Redux.demo_window state (fun m -> print_endline ("State: " ^ String.concat " " m.todos)) app
