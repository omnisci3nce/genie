open Genie
open Maths
open Styles

[@@@warnerror "-unused-value-declaration"]

type model = { show_list : bool; topleft : bool; bottomleft : bool; right_column_btns : int list }
[@@deriving show, lens]

let initial_model =
  { show_list = true; topleft = false; bottomleft = false; right_column_btns = [ 1; 2; 3; 4 ] }

let my_button_style =
  default_box |> with_color Color.stone800 |> with_hovered_color Color.stone700
  |> with_pressed_color Color.stone900

let red_button_style =
  default_box |> with_color Color.red700 |> with_hovered_color Color.red800
  |> with_pressed_color Color.red900

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
  and bottomleft_btn =
    Button.make ~text:"Remove Row" ~styles:red_button_style "btn3" (fun model ->
        if List.length model.right_column_btns = 0 then (
          print_endline "List is empty.";
          model)
        else
          {
            model with
            bottomleft = not model.bottomleft;
            right_column_btns =
              CCList.tail_opt model.right_column_btns |> CCOption.get_or ~default:[];
          })
  in
  let right_col_btns =
    List.map
      (fun i ->
        Button.make
          ~text:(" Button " ^ string_of_int i)
          ~styles:my_button_style (string_of_int i)
          (fun model ->
            let filtered = List.filter (fun btn -> btn != i) model.right_column_btns in
            { model with right_column_btns = filtered }))
      model.right_column_btns
  in

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

let _screen_width = 1000
let _screen_height = 800

let rec main_loop prev_ui ui widget_cache model n =
  let open Input in
  (* Printf.printf "Frame %d\n" n; *)
  match Djinn_sys.window_should_close () with
  | true -> n
  | false ->
      Djinn_sys.frame_begin ();
      (* Djinn.draw_text_string ~params:(Djinn_wrapper.text_params 400 400 "Hello!" Color.bright_blue); *)
      let mouse_input = Input.get_mouse_input () in
      (* Printf.printf "Mouse: (%d, %d)\n" mouse_input.x mouse_input.y; flush stdout; *)
      let new_model =
        Ui.update_ui mouse_input { keys = [] (* TODO *) } widget_cache model prev_ui
      in
      if new_model != model then (
        Printf.printf "Updated Model: %s\n" (show_model new_model);
        flush stdout)
      else ();
      let new_ui = build_ui_tree new_model in
      Ui.layout_ui 0 0 ui;
      Ui.draw_ui ui widget_cache;

      Djinn_sys.frame_end ();
      main_loop ui new_ui widget_cache new_model (n + 1)

let () =
  let open Djinn_sys in
  print_endline "I dream of Genie!\n";
  djinn_try_init ();
  let initial_ui = build_ui_tree initial_model in
  let initial_widget_cache = Hashtbl.create 10 in
  let _ = main_loop initial_ui initial_ui initial_widget_cache initial_model 0 in
  print_endline "Closed."
