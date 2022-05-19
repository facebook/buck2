#include <cstdint>

#include "a.h"
#include "b.h"

extern "C" int32_t rustlib_returns_a_known_constant();

int main() {
  return a::returnsAKnownConstant() + b::returnsAKnownConstant() +
      rustlib_returns_a_known_constant();
}
