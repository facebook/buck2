#include <string>

int lib() {
  // If weak symbols from asan stubs dependency are linked into this library
  // statically, that's going to throw.
  char buf[8192] = "hello";
  std::string str(buf);
  if (str.c_str()[0] != 'h') {
    throw -1;
  }
  return 0;
}
