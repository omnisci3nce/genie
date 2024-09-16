(* automatically generated by ocaml-bindgen 0.0.1 *)

type lifetime =
  | Function
      (** The value can live for the lifetime of the function call, which upon return will signal that the 
                value can be dropped (finalizer?) *)
  | Ocaml  (** The value is managed by the OCaml runtime *)
  | C  (** The value is allocated and passed to C which is then in charge of cleaning it up *)

type 'a cptr = { lifetime : lifetime; addr : nativeint }

external bindgen_alloc : size:int -> nativeint = "bindgen_alloc"
external bindgen_free : nativeint -> unit = "bindgen_free"
external bindgen_alloc_string : string -> nativeint = "bindgen_alloc_string"

let sizeof _ = 4 (* TODO: how to handle different types? *)

let create_ptr (value : 'a) : 'a cptr =
  let addr = bindgen_alloc ~size:(sizeof value) in
  print_endline ("Addr: " ^ Nativeint.to_string addr);
  Gc.finalise bindgen_free addr;
  { lifetime = Ocaml; addr }

let make_cstr (s : string) : char cptr =
  let addr = bindgen_alloc_string s in
  { lifetime = Ocaml; addr }

external djinn_try_init : unit -> unit = "caml_djinn_try_init"
external frame_begin : unit -> unit = "caml_frame_begin"
external frame_end : unit -> unit = "caml_frame_end"
external window_should_close : unit -> bool = "caml_window_should_close"

type nonrec box_params = {
  x : int;
  y : int;
  width : int;
  height : int;
  r : float;
  g : float;
  b : float;
}

type nonrec text_params = { x : int; y : int; r : float; g : float; b : float }

external draw_rectangle : params:box_params cptr -> unit = "caml_draw_rectangle"
external caml_box_params_of_value : params:box_params -> nativeint = "caml_box_params_of_value"
external draw_text_string : params:text_params cptr -> unit = "caml_draw_text_string"
external draw_text_str : contents:char cptr -> y:int -> x:int -> unit = "caml_draw_text_str"
external get_mouse_x : unit -> int = "caml_get_mouse_x"
external get_mouse_y : unit -> int = "caml_get_mouse_y"
external get_left_btn : unit -> bool = "caml_get_left_btn"
external get_prev_left_btn : unit -> bool = "caml_get_prev_left_btn"