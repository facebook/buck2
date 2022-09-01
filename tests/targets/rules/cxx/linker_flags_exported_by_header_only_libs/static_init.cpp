#include <stdio.h>

struct Foo {
  Foo() {
    printf("success\n");
  }
};

static Foo foo;

extern "C" void func() {}
