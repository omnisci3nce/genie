open Maths

type basic_interactable = Inactive | Hovered | Pressed
type flex_direction = Column | Row
type widget_state = Button of basic_interactable
type widget_id = string
type widget_cache = (widget_id, widget_state) Hashtbl.t

(** A "thing" that can be drawn, or affects the layout of,
    the drawing parameterised over the user's data model *)
type 'model drawable =
  | Widget of {
      name : string;
      size : unit -> unit; (* TODO *)
      draw : rect -> widget_cache -> unit;
      handle_interaction :
        Input.mouse_input -> Input.key_input -> widget_cache -> 'model -> 'model;
    }
  | Flex of { dir : flex_direction }
