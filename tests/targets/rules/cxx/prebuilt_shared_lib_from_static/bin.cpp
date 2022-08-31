#include <stdio.h>

int count = 0;

void foo();

int main() {
  foo();
  // Count should be one, from the single init call from the shared lib dep.
  return count != 1;
}
