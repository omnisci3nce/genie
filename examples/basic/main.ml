open Genie

[@@@warnerror "-unused-value-declaration"]

type example_model = {
  boolean : bool;
      (* topleft : bool; *)
      (* topright : bool;
         bottomleft : bool;
         bottomright : bool; *)
}

let initial_model =
  {
    boolean = false;
    (* topleft = false; *)
    (* topright = false;
       bottomleft = false;
       bottomright = false *)
  }

let build_ui_tree =
  let click_me =
    Button.make ~text:"Hello!" "btn1" (fun model ->
        print_endline "You did it! You clicked me!";
        { boolean = not model.boolean })
  in
  let initial_widget_state = Hashtbl.create 10 in
  (Ui.(Flex { dir = Row; children = [ click_me ] }), initial_widget_state)


let screen_width = 1000
let screen_height = 800

let rec main_loop ui widget_cache model n =
  let open Input in
  (* Printf.printf "Frame %d\n" n; *)
  match Djinn.window_should_close () with
  | true -> n
  | false ->
      Djinn.frame_begin ();
      let mouse_input = Input.get_mouse_input () in
      (* Printf.printf "Mouse: (%d, %d)\n" mouse_input.x mouse_input.y; flush stdout; *)
      let new_model, new_widget_cache =
        Ui.update_ui mouse_input { keys = [] (* TODO *) } widget_cache model ui
      in
      Ui.layout_ui screen_width screen_height ui;
      Ui.draw_ui ui new_widget_cache;

      Djinn.frame_end ();
      main_loop ui new_widget_cache new_model (n + 1)

let () =
  let open Djinn in
  print_endline "I dream of Genie!\n";
  djinn_try_init ();
  let ui, widget_cache = build_ui_tree in
  let _ = main_loop ui widget_cache initial_model 0 in
  print_endline "Closed."

        (* Djinn.draw_rectangle ~params:(box_params 100 100 100 100 Color.cyan);
         Djinn.draw_rectangle ~params:(box_params 250 100 100 100 Color.yellow);
         Djinn.draw_rectangle ~params:(box_params 100 250 100 100 Color.magenta);
         Djinn.draw_rectangle ~params:(box_params 250 250 100 100 Color.red); *)