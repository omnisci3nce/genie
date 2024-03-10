#pragma once
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SCR_WIDTH 1000
#define SCR_HEIGHT 800

/// --- Type aliases

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;

/// --- Maths

typedef struct vec2f {
    float x, y;
} vec2f;

typedef struct vec2i {
    int x, y;
} vec2i;

typedef struct vec3 {
    float x, y, z;
} vec3;

typedef struct mat4 {
  f32 data[16];
} mat4;

typedef struct mat2x2 {
  f32 data[4];
} mat2x2;

/** @brief 2D affome transformation */
typedef struct transform2d {
  vec2f position;
  f32 rotation;
  vec2f scale;
  bool is_dirty;
} transform2d;

// TODO: transform2d -> mat2x2

static inline mat4 mat4_ident() {
  return (mat4){ .data = { 1.0, 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1.0 } };
}

/** @brief Creates an orthographic projection matrix */
static inline mat4 mat4_orthographic(f32 left, f32 right, f32 bottom, f32 top, f32 near_clip,
                                     f32 far_clip) {
  // source: kohi game engine.
  mat4 out_matrix = mat4_ident();

  f32 lr = 1.0f / (left - right);
  f32 bt = 1.0f / (bottom - top);
  f32 nf = 1.0f / (near_clip - far_clip);

  out_matrix.data[0] = -2.0f * lr;
  out_matrix.data[5] = -2.0f * bt;
  out_matrix.data[10] = 2.0f * nf;

  out_matrix.data[12] = (left + right) * lr;
  out_matrix.data[13] = (top + bottom) * bt;
  out_matrix.data[14] = (far_clip + near_clip) * nf;

  return out_matrix;
}

/// --- Colors

#define FOREGROUND ((vec3){ 0.95, 0.95, 0.95 })
#define BACKGROUND ((vec3){ 0.1098, 0.0980, 0.0902 })

/// --- File IO

// TODO: return some sort of file/string result struct with an is_valid field

static const char* string_from_file(const char *path) {
  FILE *f = fopen(path, "rb");
  if (f == NULL) {
    // ERROR("Error reading file: %s. errno: %d", path, errno);
    return NULL;
  }
  if (ferror(f)) {
    // ERROR("Error reading file: %s. errno: %d", path, errno);
    return NULL;
  }
  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  rewind(f);

  char *string = malloc(fsize + 1);
  size_t result = fread(string, fsize, 1, f);
  if (result != 1) {
    printf("Unexpected EOF\n");
    // TODO: Error logs
  } else if (ferror(f)) {
    // TODO: Error logs
  }
  fclose(f);

  string[fsize] = '\0';

  return string;
}