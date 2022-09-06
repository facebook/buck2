#include <iostream>

void* make_once() {
  std::cerr << "initialized!" << std::endl;
  return NULL;
}

extern "C" {
void once() {
  static void* x = make_once();
}

void from_lib() {
  std::cerr << "from lib" << std::endl;
  once();
}

void from_main() {
  std::cerr << "from main" << std::endl;
  once();
}
}
