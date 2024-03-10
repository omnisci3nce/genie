#pragma once

#include "std.h"
#include "render.h"

#include <stb_truetype.h>

typedef struct font {
    const char* name;
    stbtt_fontinfo stbtt_font;
    stbtt_bakedchar c_data[96];
    u32 bitmap_tex_id;
} font;

typedef struct text_state {
    font default_font;
    u32 glyph_shader;
    u32 vbo, vao;
    draw_text_darray* draw_cmd_buf;
} text_state;

void text_system_render(text_state* text);

// --- Lifecycle
bool text_system_init(text_state* text);
// TODO: text_system_shutdown

// --- Drawing
// see djinn_export.h