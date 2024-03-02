#pragma once
#include "std.h"
#include "darray.h"
#include "ui.h"

struct GLFWwindow;

typedef struct draw_rect {
    int x, y, width, height;
    // TODO: f32 border_radius;
    float r, g, b; // TODO: use a vec3
} draw_rect;

DECL_TYPED_ARRAY(draw_rect)

typedef struct renderer {
    struct GLFWwindow* window;
    u32 vbo, vao, ibo;
    u32 rect_shader;
    draw_rect_darray* draw_cmd_buf;
} renderer;

bool renderer_init(renderer* ren);

u32 shader_create_separate(const char* vert_shader, const char* frag_shader);