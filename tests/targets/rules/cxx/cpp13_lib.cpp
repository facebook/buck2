#include <folly/concurrency/CacheLocality.h>

int go() {
  return folly::AccessSpreader<std::atomic>::current(1);
}
