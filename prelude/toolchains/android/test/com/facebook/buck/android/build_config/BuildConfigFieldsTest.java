/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.build_config;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.testutil.MoreAsserts;
import com.google.common.collect.ImmutableList;
import org.junit.Test;

public class BuildConfigFieldsTest {

  @Test
  public void testParseFields() {
    BuildConfigFields buildConfigValues =
        BuildConfigFields.fromFieldDeclarations(
            ImmutableList.of(
                "boolean DEBUG = false",
                "String KEYSTORE_TYPE = \"inhouse\"",
                "long MEANING_OF_LIFE = 42"));
    MoreAsserts.assertIterablesEquals(
        ImmutableList.of(
            BuildConfigFields.Field.of("boolean", "DEBUG", "false"),
            BuildConfigFields.Field.of("String", "KEYSTORE_TYPE", "\"inhouse\""),
            BuildConfigFields.Field.of("long", "MEANING_OF_LIFE", "42")),
        buildConfigValues);
  }

  @Test
  public void testToString() {
    BuildConfigFields buildConfigValues =
        BuildConfigFields.fromFieldDeclarations(
            ImmutableList.of(
                "boolean DEBUG = false",
                "String KEYSTORE_TYPE = \"inhouse\"",
                "long MEANING_OF_LIFE = 42"));
    assertEquals(
        "boolean DEBUG = false;String KEYSTORE_TYPE = \"inhouse\";long MEANING_OF_LIFE = 42",
        buildConfigValues.toString());
  }

  @Test
  public void testFieldToString() {
    assertEquals(
        "String KEYSTORE_TYPE = \"inhouse\"",
        BuildConfigFields.Field.of("String", "KEYSTORE_TYPE", "\"inhouse\"").toString());
  }
}
