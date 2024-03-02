/**
 * These are the slightly restrictive bindings for 'djinn' that makes things a lot easier
 * on 'bindgen' **for now**.
*/
#pragma once
#include <stdbool.h>

/* If my struct is too small it literally tells me that it can't decide whether 
   it should be boxed or not and we don't support that in bindgen yet so for now
   I've just padded the struct..... 
   Also, ocaml-bindgen doesn't work with no parameter function calls so 
   that's also something we need to go in and add! */
typedef struct dummy {
    int a, b, c;
} dummy;

void djinn_try_init(dummy* d);

void frame_begin(dummy* d);
void frame_end(dummy* d);
bool window_should_close(dummy* d);

typedef struct params {
    int x, y, width, height;
    float r, g, b;
} params;

void draw_rectangle(params* params);