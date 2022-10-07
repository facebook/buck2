#include <stdio.h>

size_t magic(size_t);

int main() {
  printf("Hello from C and Rust: %zu!\n", magic(21));
  return 0;
}
