#include <stdio.h>
#include "djinn.h"
#include "render.h"

#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"

Djinn g_djinn;

void djinn_try_init() {
    if (!renderer_init(&g_djinn.render)) {
        printf("Failed to initialise Renderer\n");
        exit(1);
    }
    if (!text_system_init(&g_djinn.text)) {
        printf("Failed to initialise Text system\n");
        exit(1);
    }
    g_djinn.input.window = g_djinn.render.window;
    printf("Successfully initialised Renderer!\n");
}
