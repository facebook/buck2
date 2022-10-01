int main() {
  int a = 0;
  {
    int a = 45;
    a += 1;
  }
  return a;
}
