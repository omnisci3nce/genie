open Maths

type axis_constraint = Fixed of int | PercentOfParent of float | FromTextContent of string
type size_constraint = { x_axis : axis_constraint; y_axis : axis_constraint }
type computed_size = { xy_constraints : size_constraint; final_rect : rect }
