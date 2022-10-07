// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

class B {
  fun foo() {
    A().bar()
  }

  fun bar() {}

  internal fun internalFoo() {}
}
