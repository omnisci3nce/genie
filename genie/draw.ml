let draw_rectangle x y w h c =
  let open Djinn_sys in
  let addr = caml_box_params_of_value ~params:(Djinn_wrapper.box_params x y w h c) in
  let ptr = { lifetime = Ocaml; addr } in
  draw_rectangle ~params:ptr

let draw_text x y text _c =
  let open Djinn_sys in
  let s = make_cstr text in
  draw_text_str ~contents:s ~y:x ~x:y

(* Djinn_sys.draw_rectangle ~params:(Djinn_wrapper.box_params x y w h c) *)
(* let draw_text _x _y _text _c = () Djinn_sys.draw_text_string ~params:(Djinn_wrapper.text_params x y text c) *)
