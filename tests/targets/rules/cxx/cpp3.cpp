#include <folly/portability/Config.h>
#include <stdio.h>

int main() {
  printf("Hello World from %s!\n", FOLLY_PACKAGE_STRING);
  return 0;
}
