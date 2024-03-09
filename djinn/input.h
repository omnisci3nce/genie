#pragma once
#include "std.h"

#define KEYS_MAX 256

struct GLFWwindow;

typedef struct mouse_state {
    bool prev_left_btn_pressed;
    bool left_btn_pressed;
} mouse_state;

typedef struct input_state {
    struct GLFWwindow* window;
    mouse_state mouse;
    bool pressed_keys[KEYS_MAX];
    bool just_pressed_keys[KEYS_MAX];
    bool just_released_keys[KEYS_MAX];
} input_state;

void input_update(input_state* input);

typedef enum keycode {
    ESC
} keycode;

/** @brief `key` is currently being held down */
bool key_is_pressed(keycode key);

/** @brief `key` was just pressed */
bool key_just_pressed(keycode key);

/** @brief `key` was just released */
bool key_just_released(keycode key);