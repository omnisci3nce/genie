/**
 * Imports all other headers to make an amalgamation header.
*/
#pragma once

#include "std.h"
#include "render.h"
#include "input.h"
#include "text.h"

typedef struct Djinn {
    renderer render;
    input_state input;
    text_state text;
} Djinn;

// Use a global singleton to avoid having to keep a single pointer to a Djinn in C and in OCaml
// as well as threading it through the call stack.
extern Djinn g_djinn;

void djinn_try_init(void);