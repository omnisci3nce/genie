open Genie

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

let _build_ui_tree =
  let click_me =
    Button.make ~text:"Hello!" "btn1" (fun model ->
        print_endline "You did it! You clicked me!";
        { boolean = not model.boolean })
  in
  (initial_model, Ui.(Flex { dir = Row; children = [| click_me |] }))

let box_params x y width height (color : Maths.Vec3f.t) : Djinn.params =
  { x; y; width; height; r = color.x; g = color.y; b = color.z }

let rec main_loop n =
  (* Printf.printf "Frame %d\n" n; *)
  match Djinn.window_should_close () with
  | true -> n
  | false ->
      Djinn.frame_begin ();
      Djinn.draw_rectangle ~params:(box_params 100 100 100 100 Color.cyan);
      Djinn.draw_rectangle ~params:(box_params 250 100 100 100 Color.yellow);
      Djinn.draw_rectangle ~params:(box_params 100 250 100 100 Color.magenta);
      Djinn.draw_rectangle ~params:(box_params 250 250 100 100 Color.red);
      Djinn.frame_end ();
      main_loop (n + 1)

let () =
  let open Djinn in
  print_endline "I dream of Genie!\n";
  djinn_try_init ();
  print_endline "next";
  let _ = main_loop 0 in
  print_endline "Closed."

(* let x_axis : Djinn.vec2f = { x = 1.; y = 0. } in
   Printf.printf "Here is a vector:\n\tX axis: (%f, %f)\n" x_axis.x x_axis.y;
   let position : Djinn.vec2i = { x = 100; y = 150 } in
   Printf.printf "We can draw shapes at some position, p: (%d, %d)\n" position.x
     position.y *)
