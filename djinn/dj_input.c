#include "input.h"
#include "djinn.h"

#include "GLFW/glfw3.h"

int get_mouse_x() {
    Djinn dj = g_djinn;
    double x, y;
    glfwGetCursorPos(dj.render.window, &x, &y);
    return (int)x;
}
int get_mouse_y() {
    Djinn dj = g_djinn;
    double x, y;
    glfwGetCursorPos(dj.render.window, &x, &y);
    return (int)y;
}
bool get_left_btn() {
    return false;
}
bool get_prev_left_btn() {
    return false;
}