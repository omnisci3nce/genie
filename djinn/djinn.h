/**
 * Imports all other headers to make an amalgamation header.
*/
#pragma once

#include "std.h"

#include <stddef.h>

typedef struct test_struct {
    size_t n;
} test_struct;

// #include "ui.h" // TODO: bindgen - elaborated type for 'struct ...' syntax and support 'union`s

// typedef struct djinn {
//     draw_rect_darray* draw_cmd_buf;
// } djinn;