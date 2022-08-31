#include <caml/mlvalues.h>
#include <stdio.h>

CAMLprim value caml_print_hello(value unit) {
  (void)unit;
  printf("Hello\n");
  return Val_unit;
}
