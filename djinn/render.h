#pragma once
#include "std.h"

struct GLFWwindow;

typedef struct renderer {
    struct GLFWwindow* window;
    u32 vbo, vao, ibo;
    u32 rect_shader;
} renderer;

bool renderer_init(renderer* ren);

u32 shader_create_separate(const char* vert_shader, const char* frag_shader);