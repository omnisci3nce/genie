/**
 * Imports all other headers to make an amalgamation header.
*/
#pragma once

#include "std.h"
#include "render.h"
#include "input.h"
// #include "ui.h" // FIXME: bindgen - elaborated type for 'struct ...' syntax and support 'union`s


typedef struct Djinn {
    renderer render;
    input_state input;
} Djinn;

void djinn_try_init(Djinn* dj);