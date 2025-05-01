/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

public @interface AnnotationWithSingleClassForTreeBackedTest {
  Class<?> single();

  String SOURCE_CODE =
      "package com.facebook.buck.jvm.java.abi.source;\n"
          + "public @interface AnnotationWithSingleClassForTreeBackedTest {\n"
          + "Class<?> single();\n"
          + "}";
}
