/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.java;

import com.facebook.buck.jvm.cd.serialization.SerializationUtil;
import com.facebook.buck.jvm.java.JdkProvidedInMemoryJavac;
import com.facebook.buck.jvm.java.ResolvedJavac;
import com.facebook.infer.annotation.Nullsafe;
import com.google.common.collect.ImmutableList;
import com.google.protobuf.ProtocolStringList;

/** {@link ResolvedJavac} to protobuf serializer */
@Nullsafe(Nullsafe.Mode.LOCAL)
public class ResolvedJavacSerializer {

  private ResolvedJavacSerializer() {}

  /**
   * Deserializes javacd model's {@link com.facebook.buck.cd.model.java.ResolvedJavac} into {@link
   * ResolvedJavac}.
   */
  public static ResolvedJavac deserialize(com.facebook.buck.cd.model.java.ResolvedJavac javac) {
    var javacCase = javac.getJavacCase();
    switch (javacCase) {
      case EXTERNALJAVAC:
        throw new IllegalArgumentException("ExternalJavac in CD worker is not supported");

      case JSR199JAVAC:
        return JdkProvidedInMemoryJavac.createJsr199Javac();

      case JAVAC_NOT_SET:
      default:
        throw SerializationUtil.createNotSupportedException(javacCase);
    }
  }

  private static ImmutableList<String> toImmutableList(ProtocolStringList list) {
    return ImmutableList.copyOf(list);
  }
}
