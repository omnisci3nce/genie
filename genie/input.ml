type mouse_input = {
  x: int;
  y: int;
}

type key_transition = Noop | JustPressed | JustReleased
type key = Esc | Up | Left | Right | Down  (* ... etc *)
type key_input = {
  keys: (key * key_transition) list
}