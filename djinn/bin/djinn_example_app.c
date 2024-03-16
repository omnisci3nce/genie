#include "djinn.h"

#include "glad/glad.h"
#include "GLFW/glfw3.h"

int main() {
    printf("Welcome to your first Djinn loop.\n");
    djinn_try_init();

    vec3 bg = BACKGROUND;
    vec3 cyan = (vec3) { 0.063, 0.8, 0.835 };

    while (!glfwWindowShouldClose(g_djinn.render.window)) {
        glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glClearColor(bg.x, bg.y, bg.z, 1.0f);

        // Try drawing a rectangle

        glfwSwapBuffers(g_djinn.render.window);
        glfwPollEvents();
    }

    return 0;
}