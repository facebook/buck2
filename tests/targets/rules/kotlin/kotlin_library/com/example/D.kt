// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

package com.example2

import B
import C

class D {
  fun useInternal() {
    B().internalFoo()
    C().internalFoo()
  }
}
