#pragma once
#include "std.h"
#include "darray.h"

typedef struct draw_rect {
    i32 x, y;
    f32 border_radius;
    // TODO: color
} draw_rect;

DECL_TYPED_ARRAY(draw_rect)