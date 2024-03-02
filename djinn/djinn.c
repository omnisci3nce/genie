#include <stdio.h>
#include "djinn.h"
#include "render.h"

Djinn g_djinn;

void djinn_try_init(int* flags) {
    // g_djinn.render - ;
    if (!renderer_init(&g_djinn.render)) {
        printf("Failed to initialise Renderer\n");
        exit(1);
    }
    printf("Successfully initialised Renderer!\n");
}
