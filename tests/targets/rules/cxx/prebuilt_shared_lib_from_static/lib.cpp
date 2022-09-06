extern int count;

struct Init {
  Init() {
    count++;
  }
};

Init init;

void foo() {}
