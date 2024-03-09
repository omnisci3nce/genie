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