// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import com.google.common.collect.ImmutableSet

class C {
  fun useA(unused: ImmutableSet<Integer>) {
    A().foo()
  }

  fun useB() {
    B().foo()
  }

  internal fun internalFoo() {}
}
