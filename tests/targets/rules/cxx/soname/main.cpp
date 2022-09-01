// Copyright 2004-present Facebook. All Rights Reserved.

#include <dlfcn.h>
#include <stdio.h>

int main(int argc, char** argv) {
  void* handle = dlopen(argv[1], RTLD_GLOBAL | RTLD_LAZY);
  if (handle == NULL) {
    fprintf(stderr, "%s\n", dlerror());
    return 1;
  }
  return 0;
}
