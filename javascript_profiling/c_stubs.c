#include <caml/mlvalues.h>

// This file is just here to placate the native compiler, which
// is still run on javascript-only executables for some reason.

CAMLprim value js_prof_mark(value name) {
  // (void) "uses" a value, so we don't trigger the
  // unused-variables error message.
  (void) name;
  return Val_unit;
}

CAMLprim value js_prof_measure(value name, value before, value after) {
  (void) name;
  (void) before;
  (void) after;
  return Val_unit;
}

CAMLprim value js_prof_clear_marks(value u) {
  (void) u;
  return Val_unit;
}

CAMLprim value js_prof_clear_measures(value u) {
  (void) u;
  return Val_unit;
}
