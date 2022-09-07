void local() {}

void local_exported() {}

void global_func() {
  local();
  local_exported();
}
