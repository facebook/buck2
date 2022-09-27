// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

extern "C" void from_main();
extern "C" void via_lib();

int main() {
  from_main();
  via_lib();
  return 0;
}
