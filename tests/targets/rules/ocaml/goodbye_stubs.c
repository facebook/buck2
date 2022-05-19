#include <caml/mlvalues.h>
#include <stdio.h>

CAMLprim value caml_print_goodbye(value unit) {
  (void)unit;
  printf("Goodbye\n");
  return Val_unit;
}
