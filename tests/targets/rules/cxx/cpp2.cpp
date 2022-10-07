#include <folly/ConstexprMath.h>
#include <stdio.h>

int main() {
  printf("Hello World from %i!\n", folly::constexpr_min(1, 2));
  return 0;
}
