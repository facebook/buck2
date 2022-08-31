#include "foo.h"

#if FOO
int one() {
  return 1;
}
#endif

#if BAR
int two() {
  return 2;
}
#endif
