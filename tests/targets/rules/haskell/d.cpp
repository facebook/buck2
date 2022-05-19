#include <gtest/gtest.h>

#include <HsFFI.h>
#include <Rts.h>

#if BUCK2
#include <Lib/Z_stub.h>
#else
extern "C" {
extern int hs_g(int);
}
#endif

TEST(HaskellRuntime, DefaultInit) {
  EXPECT_EQ(hs_g(3), 5);
}

int main(int argc, char* argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  hs_init(&argc, &argv);
  auto ret = RUN_ALL_TESTS();
  hs_exit();
  return ret;
}
