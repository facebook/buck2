#include <foo/public.h>

int bar();

int main() {
  return foo() * bar();
}
