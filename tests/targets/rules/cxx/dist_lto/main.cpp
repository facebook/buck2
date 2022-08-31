#include "a.h"
#include "b.h"

int main() {
  return a::returnsAKnownConstant() + b::returnsAKnownConstant();
}
