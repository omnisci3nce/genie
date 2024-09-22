let rec repeat n f =
  match n with
  | 0 -> ()
  | a ->
      f ();
      repeat (a - 1) f
