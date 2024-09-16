#include "text.h"
#include "render.h"

#include "glad/glad.h"

bool font_bitmap_texture_create(font *font, u8 *pixel_data) {
  glGenTextures(1, &font->bitmap_tex_id);
  glBindTexture(GL_TEXTURE_2D, font->bitmap_tex_id);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 512, 512, 0, GL_RED, GL_UNSIGNED_BYTE, pixel_data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST_MIPMAP_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 8);
  glHint(GL_GENERATE_MIPMAP_HINT, GL_NICEST);
  glGenerateMipmap(GL_TEXTURE_2D);
  return true;
}

unsigned char ttf[1 << 20];

void uniform_mat4f(u32 program_id, const char *uniform_name, mat4 *value) {
  glUniformMatrix4fv(glGetUniformLocation(program_id, uniform_name), 1, GL_FALSE, value->data);
}

bool text_system_init(text_state *text) {
  printf("Text init\n");

  // compiler shader
  printf("Load text_system's glyph drawing shader\n");
  text->glyph_shader =
      shader_create_separate("djinn/shaders/glyph.vert", "djinn/shaders/glyph.frag");

  // create VBO
  glGenBuffers(1, &text->vbo);
  glGenVertexArrays(1, &text->vao);

  glBindBuffer(GL_ARRAY_BUFFER, text->vbo);
  glBindVertexArray(text->vao);

  // setup vertex attributes
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void *)0);  // position
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float),
                        (void *)(2 * sizeof(float)));  // texture uv
  glEnableVertexAttribArray(1);

  // allocate space
  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 6 * 4, NULL, GL_DYNAMIC_DRAW);

  // reset buffer
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  // load font
  u8 *temp_bitmap = malloc(512 * 512);

  font default_font = { .name = "Inter" };
  char *ttf = string_from_file("djinn/vendor/Inter-4.0/InterVariable.ttf");
  stbtt_BakeFontBitmap(ttf, 0, 32.0, temp_bitmap, 512, 512, 32, 96,
                       default_font.c_data);  // no guarantee this fits!

  font_bitmap_texture_create(&default_font, temp_bitmap);

  // just doublechecking
//   stbi_write_png("_temp/inter_bitmap.png", 512, 512, 1, temp_bitmap, 0);

  text->draw_cmd_buf = draw_text_darray_new(50);

  free(ttf);
  free(temp_bitmap);
  text->default_font = default_font;
  return true;
}

// typedef struct ui_bound {
//   int min, mid, max, ext;
// } ui_bound;

// static f32 scale_pt(f32 value, struct ui_bound from, struct ui_bound to) {
//   return (to.max - to.min) * (value - from.min) / (from.max - from.min) + to.min;
// }

typedef struct glyph_quad_vertex {
  vec2f pos;
  vec2f uv;
} glyph_quad_vertex;

// void draw_text(text_state* text, f32 xin, f32 yin, char *contents) {
//   draw_text p = { .contents = contents, .x = xin, .y = yin };
//   draw_text_darray_push_copy(text.draw_cmd_buf, &p);
// }

void text_system_render(text_state *text) {
  // CASSERT(xin >= 0.0 && xin <= SCR_WIDTH);
  // CASSERT(yin >= 0.0 && xin <= SCR_HEIGHT);

  // text_system_state text = core->text;
  font font = text->default_font;

  mat4 proj = mat4_orthographic(0.0, SCR_WIDTH, SCR_HEIGHT, 0.0, -1.0, 1.0);

  glUseProgram(text->glyph_shader);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, text->default_font.bitmap_tex_id);
  glBindVertexArray(text->vao);
  glBindBuffer(GL_ARRAY_BUFFER, text->vbo);

  uniform_mat4f(text->glyph_shader, "projection", &proj);

  for (int i = 0; i < text->draw_cmd_buf->len; i++) {
    draw_text p = text->draw_cmd_buf->data[i];
    // CASSERT(p.contents != NULL);
    // CASSERT(p.x >= 0.0 && p.x <= SCR_WIDTH);
    // CASSERT(p.y >= 0.0 && p.y <= SCR_HEIGHT);
    // DEBUG("Drawing text %f %f %s", p.x, p.y, p.contents);

    f32 x = p.x;
    f32 y = p.y;
    char *contents = p.contents;
    while (*contents) {
      if (*contents >= 32 && *contents < 128) {  // valid ASCII characters
        glyph_quad_vertex verts[4];
        stbtt_aligned_quad q;
        // printf("Current X, Y : %f %f\n", x, y);
        stbtt_GetBakedQuad(font.c_data, 512, 512, *contents - 32, &x, &y, &q, 1);

        // top-left
        verts[0].pos = (vec2f){ .x = q.x0, .y = q.y0 };
        verts[0].uv = (vec2f){ .x = q.s0, .y = q.t0 };
        // top-right
        verts[1].pos = (vec2f){ .x = q.x1, .y = q.y0 };
        verts[1].uv = (vec2f){ .x = q.s1, .y = q.t0 };
        // bottom-right
        verts[2].pos = (vec2f){ .x = q.x1, .y = q.y1 };
        verts[2].uv = (vec2f){ .x = q.s1, .y = q.t1 };
        // bottom-left
        verts[3].pos = (vec2f){ .x = q.x0, .y = q.y1 };
        verts[3].uv = (vec2f){ .x = q.s0, .y = q.t1 };

        // printf("Top-left x: %f y: %f s: %f t: %f \n", verts[0].pos.x, verts[0].pos.y,
        // verts[0].uv.x,
        //        verts[0].uv.y);
        // printf("Top-right x: %f y: %f s: %f t: %f \n", verts[1].pos.x, verts[1].pos.y,
        // verts[1].uv.x,
        //        verts[1].uv.y);
        // printf("Bottom-right x: %f y:%f s: %f t: %f \n", verts[2].pos.x, verts[2].pos.y,
        //        verts[2].uv.x, verts[2].uv.y);
        // printf("Bottom-left x: %f y: %f s: %f t: %f \n", verts[3].pos.x, verts[3].pos.y,
        //        verts[3].uv.x, verts[3].uv.y);
        glyph_quad_vertex dup_verts[6];
        size_t indices[] = { 3, 2, 0, 2, 1, 0 };
        for (size_t v = 0; v < 6; v++) {
          dup_verts[v] = verts[indices[v]];
        }

        size_t size = sizeof(glyph_quad_vertex) * 6;
        glBufferData(GL_ARRAY_BUFFER, size, dup_verts, GL_DYNAMIC_DRAW);
        glDrawArrays(GL_TRIANGLES, 0, 6);
      }
      ++contents;
    }
  }

  draw_text_darray_clear(text->draw_cmd_buf);
}

// void text_system_shutdown(text_state *text) {
//   INFO("Text system shutdown");
//   glDeleteTextures(1, &text->default_font.bitmap_tex.raw);
//   glDeleteVertexArrays(1, text->glyph_vao);
//   glDeleteBuffers(1, text->glyph_vbo);
// }