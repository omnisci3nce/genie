/**
 * These are the slightly restrictive bindings for 'djinn' that makes things a lot easier
 * on 'bindgen' **for now**.
*/
#pragma once
#include <stdbool.h>

void djinn_try_init(void);

void frame_begin();
void frame_end();
bool window_should_close();

typedef struct params {
    int x, y, width, height;
    float r, g, b;
} params;

void draw_rectangle(params* params);

// --- Input
int get_mouse_x();
int get_mouse_y();
bool get_left_btn();
bool get_prev_left_btn();