let draw_rectangle x y w h c = Djinn.draw_rectangle ~params:(Djinn_wrapper.box_params x y w h c)
let draw_text _x _y _text _c = failwith "TODO: implement text drawing in djinn"
