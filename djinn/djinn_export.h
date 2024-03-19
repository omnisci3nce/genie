/**
 * These are the slightly restrictive bindings for 'djinn' that makes things a lot easier
 * on 'bindgen' **for now**.
*/
#pragma once
#include <stdbool.h>

// --- Lifecycle
void djinn_try_init(void);

// --- Frame
void frame_begin();
void frame_end();
bool window_should_close();

// --- Drawing
typedef struct box_params {
    int x, y, width, height;
    float r, g, b;
} box_params;

typedef struct text_params {
    int x, y;
    // char* contents;
    float r, g, b;
} text_params;

void draw_rectangle(box_params* params);
void draw_text_string(text_params* params);
void draw_text_str(int x, int y, char* contents);

// --- Input
int get_mouse_x();
int get_mouse_y();
bool get_left_btn();
bool get_prev_left_btn();