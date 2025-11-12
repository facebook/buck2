/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.unarchive;

import org.junit.Assert;
import org.junit.Test;

public class ArchiveFormatTest {

  @Test
  public void returnsCorrectCompressionFormatsBasedOnFilename() {
    Assert.assertEquals(
        ArchiveFormat.ZIP, ArchiveFormat.getFormatFromFilename("foo.bar.zip").get());
  }

  @Test
  public void returnsEmptyWhenNoCompressionFormatCouldBeFoundBasedOnFilename() {
    Assert.assertFalse(ArchiveFormat.getFormatFromFilename("foo.bar").isPresent());
  }

  @Test
  public void returnsCorrectCompressionFormatsBasedOnShortName() {
    Assert.assertEquals(ArchiveFormat.ZIP, ArchiveFormat.getFormatFromShortName("zip").get());
  }

  @Test
  public void returnsEmptyWhenNoCompressionFormatCouldBeFoundBasedOnShortName() {
    Assert.assertFalse(ArchiveFormat.getFormatFromShortName("foo").isPresent());
  }
}
