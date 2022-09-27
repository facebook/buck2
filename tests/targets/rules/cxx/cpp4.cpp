#include <boost/uuid/uuid.hpp>
#include <stdio.h>

int main() {
  boost::uuids::uuid uuid;
  printf("Boost UUID nil %s\n", uuid.is_nil() ? "true" : "false");
  return 0;
}
