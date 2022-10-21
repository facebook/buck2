// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

#include <iostream> // this inclusion helps reproduce the -fPIC issue for distributed_thinlto.

int get_zero() {
  std::cout << "Test lib" << std::endl;
  return 0;
}
