let draw_rectangle x y w h c = Djinn.draw_rectangle ~params:(Djinn_wrapper.box_params x y w h c)
let draw_text x y text c = Djinn.draw_text_string ~params:(Djinn_wrapper.text_params x y text c)
