extern "C" int lib2_func();

extern "C" int lib1_func() {
  return lib2_func() - 1;
}
