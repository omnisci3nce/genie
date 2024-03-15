#include <stdio.h>
#include "djinn.h"
#include "render.h"

Djinn g_djinn;

void djinn_try_init(int* flags) {
    if (!renderer_init(&g_djinn.render)) {
        printf("Failed to initialise Renderer\n");
        exit(1);
    }
    g_djinn.input.window = g_djinn.render.window;
    printf("Successfully initialised Renderer!\n");
}
