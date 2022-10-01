#include "gtest/gtest.h"

TEST(Simple, Fail) {
  EXPECT_EQ(1, 2 + 1);
}
