// Copyright 2004-present Facebook. All Rights Reserved.

#include <stdlib.h>

__attribute__((__weak__)) void __my_function() {
  abort();
}

int main() {
  __my_function();
  return 0;
}
