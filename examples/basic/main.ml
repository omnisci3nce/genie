let () =
  print_endline "I dream of Genie!\n";
  let x_axis : Djinn.vec2f = { x = 1.; y = 0. } in
  Printf.printf "Here is a vector:\n\tX axis: (%f, %f)\n" x_axis.x x_axis.y;
  let position : Djinn.vec2i = { x = 100; y = 150 } in
  Printf.printf "We can draw shapes at some position, p: (%d, %d)\n"
    position.x position.y
