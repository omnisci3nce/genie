type key_transition = Noop | JustPressed | JustReleased
type key = Esc | Up | Left | Right | Down  (* ... etc *)
type key_input = {
  keys: (key * key_transition) list
}

(** Edge transition *)
type mouse_btn_action = Clicked | Held | Released | NoAction
let string_of_mouse_action = function
  | Clicked -> "Clicked"
  | Held -> "Held"
  | Released -> "Released"
  | NoAction -> "NoAction"

let mouse_btn_state_transition prev cur = match prev, cur with
  | true, true -> Held
  | true, false -> Released
  | false, true -> Clicked
  | false, false -> NoAction

type mouse_input = {
  x: int;
  y: int;
  left_button: mouse_btn_action
}

let get_mouse_input () : mouse_input =
  let mouse_pos = Djinn_wrapper.get_mouse_pos () in
  let prev_left = Djinn.get_prev_left_btn ()
  and left = Djinn.get_left_btn () in
  let left_btn_action = mouse_btn_state_transition prev_left left in
  { x = mouse_pos.x; y = mouse_pos.y; left_button = left_btn_action }