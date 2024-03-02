#include <stdio.h>
#include "djinn.h"

void djinn_try_init(Djinn* dj) {
    if (!renderer_init(&dj->render)) {
        printf("Failed to initialise Renderer\n");
        exit(1);
    }
}