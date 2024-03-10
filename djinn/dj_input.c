#include "input.h"
#include "djinn.h"

#include "GLFW/glfw3.h"

void input_update(input_state* input) {
    // buttons
    mouse_state new_mouse_state = {0};
    int left_state = glfwGetMouseButton(input->window, GLFW_MOUSE_BUTTON_LEFT);

    new_mouse_state.prev_left_btn_pressed = input->mouse.left_btn_pressed;
    if (left_state == GLFW_PRESS) {
        new_mouse_state.left_btn_pressed = true;
    } else {
        new_mouse_state.left_btn_pressed = false;
    }
    // printf("Prev Left: %d   Left: %d\n", new_mouse_state.prev_left_btn_pressed, new_mouse_state.left_btn_pressed);

    input->mouse = new_mouse_state;
}

int get_mouse_x() {
    Djinn dj = g_djinn;
    double x, y;
    glfwGetCursorPos(dj.render.window, &x, &y);
    return (int)x * 2;
}
int get_mouse_y() {
    Djinn dj = g_djinn;
    double x, y;
    glfwGetCursorPos(dj.render.window, &x, &y);
    return (int)y * 2;
}
bool get_left_btn() {
    Djinn dj = g_djinn;
    return !dj.input.mouse.left_btn_pressed;
}
bool get_prev_left_btn() {
    Djinn dj = g_djinn;
    return !dj.input.mouse.prev_left_btn_pressed;
}