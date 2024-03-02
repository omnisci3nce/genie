#include "djinn.h"

#include "glad/glad.h"
#include "GLFW/glfw3.h"

int main() {
    printf("Welcome to your first Djinn loop.\n");
    Djinn my_app;
    djinn_try_init(&my_app);

    while (!glfwWindowShouldClose(my_app.render.window)) {
        glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        vec3 bg = BACKGROUND;
        glClearColor(bg.x, bg.y, bg.z, 1.0f);

        glfwSwapBuffers(my_app.render.window);
        glfwPollEvents();
    }

    return 0;
}