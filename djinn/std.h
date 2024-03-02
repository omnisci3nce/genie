#pragma once
#include <stdint.h>

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