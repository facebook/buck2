#include <cstring>
#include <iostream>
#include <zlib.h>

int main() {
  uLong adler = adler32(0L, Z_NULL, 0);

  const char msg[] = "Hello Conan";
  adler = adler32(adler, reinterpret_cast<const Bytef *>(msg), strlen(msg));

  std::cout << adler << std::endl;
}
