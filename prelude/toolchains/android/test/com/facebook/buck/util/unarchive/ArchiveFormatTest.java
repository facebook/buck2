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
        ArchiveFormat.TAR, ArchiveFormat.getFormatFromFilename("foo.bar.tar").get());
    Assert.assertEquals(
        ArchiveFormat.TAR_BZ2, ArchiveFormat.getFormatFromFilename("foo.bar.tar.bz2").get());
    Assert.assertEquals(
        ArchiveFormat.TAR_GZ, ArchiveFormat.getFormatFromFilename("foo.bar.tar.gz").get());
    Assert.assertEquals(
        ArchiveFormat.TAR_XZ, ArchiveFormat.getFormatFromFilename("foo.bar.tar.xz").get());
    Assert.assertEquals(
        ArchiveFormat.ZIP, ArchiveFormat.getFormatFromFilename("foo.bar.zip").get());
  }

  @Test
  public void returnsEmptyWhenNoCompressionFormatCouldBeFoundBasedOnFilename() {
    Assert.assertFalse(ArchiveFormat.getFormatFromFilename("foo.bar").isPresent());
  }

  @Test
  public void returnsCorrectCompressionFormatsBasedOnShortName() {
    Assert.assertEquals(ArchiveFormat.TAR, ArchiveFormat.getFormatFromShortName("tar").get());
    Assert.assertEquals(
        ArchiveFormat.TAR_BZ2, ArchiveFormat.getFormatFromShortName("tar.bz2").get());
    Assert.assertEquals(ArchiveFormat.TAR_GZ, ArchiveFormat.getFormatFromShortName("tar.gz").get());
    Assert.assertEquals(ArchiveFormat.TAR_XZ, ArchiveFormat.getFormatFromShortName("tar.xz").get());
    Assert.assertEquals(ArchiveFormat.ZIP, ArchiveFormat.getFormatFromShortName("zip").get());
  }

  @Test
  public void returnsEmptyWhenNoCompressionFormatCouldBeFoundBasedOnShortName() {
    Assert.assertFalse(ArchiveFormat.getFormatFromShortName("foo").isPresent());
  }
}
