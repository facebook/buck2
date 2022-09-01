// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

#include <iostream>

void* make_once() {
  std::cout << "initialized" << std::endl;
  return nullptr;
}

extern "C" {
void once() {
  // @lint-ignore CLANGTIDY
  static void* v = make_once();
  std::cout << "done" << std::endl;
}

void from_lib() {
  std::cout << "from lib" << std::endl;
  once();
}

void from_main() {
  std::cout << "from main" << std::endl;
  once();
}
}
