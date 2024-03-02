#pragma once
#include "std.h"

#define KEYS_MAX 256

struct GLFWwindow;

typedef struct input_state {
    struct GLFWwindow* window;
    bool pressed_keys[KEYS_MAX];
    bool just_pressed_keys[KEYS_MAX];
    bool just_released_keys[KEYS_MAX];
} input_state;

typedef enum keycode {
    ESC
} keycode;

/** @brief `key` is currently being held down */
bool key_is_pressed(keycode key);

/** @brief `key` was just pressed */
bool key_just_pressed(keycode key);

/** @brief `key` was just released */
bool key_just_released(keycode key);