// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

package com.example

@TestAnnotation
class E {
  fun useGeneratedClass() {
    GeneratedE.foo()
    GeneratedE.methodNameFromApParams1()
    GeneratedE.methodNameFromApParams2()
  }
}
