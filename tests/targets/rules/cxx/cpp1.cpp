#include <stdio.h>

int main() {
  // Deliberately use fprintf rather than printf, because
  // fprintf participates in UBSAN hooks, so there are ASAN
  // bugs which trigger for fprint, but not printf.
  fprintf(stderr, "Hello World from C++!\n");
  return 0;
}
