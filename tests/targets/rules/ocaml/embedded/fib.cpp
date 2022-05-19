// "Advanced example with callbacks"
// https://v2.ocaml.org/manual/intfc.html#s:c-advexample

#include <iostream>
#include <string>

#include <caml/callback.h>
#include <caml/mlvalues.h>

int fib(int n) {
  static const value* fib_closure = NULL;
  if (fib_closure == NULL)
    fib_closure = caml_named_value("fib");
  return Int_val(caml_callback(*fib_closure, Val_int(n)));
}

std::string format_result(int n) {
  static const value* format_result_closure = NULL;
  if (format_result_closure == NULL)
    format_result_closure = caml_named_value("format_result");
  return String_val(caml_callback(*format_result_closure, Val_int(n)));
}

int main(int argc, char** argv) {
  caml_startup(argv);

  std::cout << "fib(10) = " << format_result(fib(10)) << std::endl;

  return 0;
}
