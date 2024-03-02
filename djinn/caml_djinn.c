/* automatically generated by ocaml-bindgen 0.0.1 */
#include "djinn.h"
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
vec2f* caml_vec2f_of_value(value caml_x) {
  vec2f* x = malloc(sizeof(struct vec2f));
  x->x = Double_val(Field(caml_x, 0));
  x->y = Double_val(Field(caml_x, 1));
  return x;
}

value caml_vec2f_to_value(struct vec2f* x) {
  CAMLparam0();
  CAMLlocal1(caml_x);
  caml_x = caml_alloc_tuple(2);
  Store_field(caml_x, 0, caml_copy_double(x->x));
  Store_field(caml_x, 1, caml_copy_double(x->y));
  CAMLreturn(caml_x);
}

vec2i* caml_vec2i_of_value(value caml_x) {
  vec2i* x = malloc(sizeof(struct vec2i));
  x->x = Int_val(Field(caml_x, 0));
  x->y = Int_val(Field(caml_x, 1));
  return x;
}

value caml_vec2i_to_value(struct vec2i* x) {
  CAMLparam0();
  CAMLlocal1(caml_x);
  caml_x = caml_alloc_tuple(2);
  Store_field(caml_x, 0, Val_int(x->x));
  Store_field(caml_x, 1, Val_int(x->y));
  CAMLreturn(caml_x);
}

char* caml_string_from_file(value caml_path) {
  CAMLparam1(caml_path);
  CAMLlocal1(result);
  char* path = caml_char_of_value(caml_path);
  result = string_from_file(path);
  CAMLreturn(result);
}

renderer* caml_renderer_of_value(value caml_x) {
  renderer* x = malloc(sizeof(struct renderer));
  x->vbo = Int_val(Field(caml_x, 0));
  x->vao = Int_val(Field(caml_x, 1));
  x->ibo = Int_val(Field(caml_x, 2));
  x->rect_shader = Int_val(Field(caml_x, 3));
  return x;
}

value caml_renderer_to_value(struct renderer* x) {
  CAMLparam0();
  CAMLlocal1(caml_x);
  caml_x = caml_alloc_tuple(4);
  Store_field(caml_x, 0, Val_int(x->vbo));
  Store_field(caml_x, 1, Val_int(x->vao));
  Store_field(caml_x, 2, Val_int(x->ibo));
  Store_field(caml_x, 3, Val_int(x->rect_shader));
  CAMLreturn(caml_x);
}

bool caml_renderer_init(value caml_ren) {
  CAMLparam1(caml_ren);
  CAMLlocal1(result);
  renderer* ren = caml_renderer_of_value(caml_ren);
  result = renderer_init(ren);
  CAMLreturn(result);
}

u32 caml_shader_create_separate(value caml_vert_shader, value caml_frag_shader) {
  CAMLparam2(caml_vert_shader, caml_frag_shader);
  CAMLlocal1(result);
  char* vert_shader = caml_char_of_value(caml_vert_shader);
  char* frag_shader = caml_char_of_value(caml_frag_shader);
  result = shader_create_separate(vert_shader, frag_shader);
  CAMLreturn(result);
}

Djinn* caml_Djinn_of_value(value caml_x) {
  Djinn* x = malloc(sizeof(struct Djinn));
  x->render = Int_val(Field(caml_x, 0));
  return x;
}

value caml_Djinn_to_value(struct Djinn* x) {
  CAMLparam0();
  CAMLlocal1(caml_x);
  caml_x = caml_alloc_tuple(1);
  Store_field(caml_x, 0, Val_int(x->render));
  CAMLreturn(caml_x);
}

void caml_djinn_try_init(value caml_dj) {
  CAMLparam1(caml_dj);
  Djinn* dj = caml_Djinn_of_value(caml_dj);
  djinn_try_init(dj);
  CAMLreturn0;
}


