#include <folly/Subprocess.h>

#include "tools/cxx/Resources.h"

int main() {
  auto path = build::getResourcePath(
      "buck2/tests/targets/rules/cxx/binary_in_resources/inner");
  folly::Subprocess({path.string()}).waitChecked();
}
