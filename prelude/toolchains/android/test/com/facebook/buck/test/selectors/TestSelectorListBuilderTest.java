/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.test.selectors;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.Test;

/** Testing parsing of test selectors to be used by TestCommand */
public class TestSelectorListBuilderTest {
  @Test
  public void addRawSelectors() {
    TestSelectorList.Builder builder = TestSelectorList.builder();
    builder.addRawSelectors("com.foo.Foo");
    builder.addRawSelectors("!com.bar.Bar");
    TestSelectorList list = builder.build();

    assertTrue(list.possiblyIncludesClassName("com.foo.Foo"));
    assertFalse(list.possiblyIncludesClassName("com.bar.Bar"));
  }

  @Test
  public void addFileSelectors() throws Exception {
    TestSelectorList.Builder builder = TestSelectorList.builder();
    Path tempFile = Files.createTempFile("test", "selectors");
    try (BufferedWriter writer = Files.newBufferedWriter(tempFile)) {
      writer.write("com.foo.Foo\n!com.bar.Bar");
    }
    builder.addRawSelectors(":" + tempFile);
    TestSelectorList list = builder.build();

    assertTrue(list.possiblyIncludesClassName("com.foo.Foo"));
    assertFalse(list.possiblyIncludesClassName("com.bar.Bar"));
  }

  @Test
  public void addSimpleSelectorWithCommaParameters() {
    TestSelectorList selectorList =
        TestSelectorList.builder().addSimpleTestSelector("Foo,bar[param,name]").build();
    assertTrue(selectorList.isIncluded(new TestDescription("Foo", "bar[param,name]")));
    assertFalse(selectorList.isIncluded(new TestDescription("Foo", "bar[paramname]")));
    assertFalse(selectorList.isIncluded(new TestDescription("Faz", "bar")));
    assertFalse(selectorList.isIncluded(new TestDescription("Foo", "baz[param,name]")));
  }
}
