#pragma once
#include "std.h"

typedef struct ui_bound {
  int min, mid, max, ext;
} ui_bound;

typedef struct ui_box {
  struct ui_bound x;
  struct ui_bound y;
} ui_box;

static f32 scale_pt(f32 value, struct ui_bound from, struct ui_bound to) {
  return (to.max - to.min) * (value - from.min) / (from.max - from.min) + to.min;
}