// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

int get_zero();

extern "C" int get_one() {
  return get_zero() + 1;
}
