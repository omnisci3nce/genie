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

