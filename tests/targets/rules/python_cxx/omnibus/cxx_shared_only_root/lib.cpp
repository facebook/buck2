extern "C" int lib2_func();

extern "C" int lib_func() {
  return lib2_func();
}
