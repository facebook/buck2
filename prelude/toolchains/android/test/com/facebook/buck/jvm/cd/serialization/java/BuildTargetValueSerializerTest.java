/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.serialization.java;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.facebook.buck.jvm.core.BuildTargetValue;
import org.junit.Test;

public class BuildTargetValueSerializerTest {
  @Test
  public void serializeDeserialize() {
    BuildTargetValue expected =
        new BuildTargetValue(
            com.facebook.buck.cd.model.java.BuildTargetValue.Type.LIBRARY, "fully_qualified_name");

    BuildTargetValue actual =
        BuildTargetValueSerializer.deserialize(BuildTargetValueSerializer.serialize(expected));

    assertThat(actual.getType(), is(expected.getType()));
    assertThat(actual.getFullyQualifiedName(), is(expected.getFullyQualifiedName()));
  }
}
