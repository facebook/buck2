// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

#include "buck2/tests/targets/rules/cxx/dist_lto/cpp_calls_rust/lib.rs"
#include <iostream>

void do_test() {
  std::cout << "cpp_library_calls_rust: " << std::boolalpha
            << same_length("rust", "test") << std::endl;
}
