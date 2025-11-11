/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.unit;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class SizeUnitTest {
  @Test
  public void testBytes() {
    assertEquals(4L, SizeUnit.BYTES.toBytes(4L));
    assertEquals(0L, SizeUnit.BYTES.toKilobytes(4L));
    assertEquals(2L, SizeUnit.BYTES.toKilobytes(2048L));
    assertEquals(10L, SizeUnit.BYTES.toMegabytes(10L * 1024L * 1024L));
    assertEquals(10L, SizeUnit.BYTES.toGigabytes(10L * 1024L * 1024L * 1024L));
    assertEquals(10L, SizeUnit.BYTES.toTerabytes(10L * 1024L * 1024L * 1024L * 1024L));
  }

  @Test
  public void testKilobytes() {
    assertEquals(2048L, SizeUnit.KILOBYTES.toBytes(2L));
    assertEquals(2048L, SizeUnit.KILOBYTES.toKilobytes(2048L));
    assertEquals(0L, SizeUnit.KILOBYTES.toMegabytes(4L));
    assertEquals(10L, SizeUnit.KILOBYTES.toMegabytes(10L * 1024L));
    assertEquals(10L, SizeUnit.KILOBYTES.toGigabytes(10L * 1024L * 1024L));
    assertEquals(10L, SizeUnit.KILOBYTES.toTerabytes(10L * 1024L * 1024L * 1024L));
  }

  @Test
  public void testMegabytes() {
    assertEquals(1024L * 1024L, SizeUnit.MEGABYTES.toBytes(1L));
    assertEquals(2048L, SizeUnit.MEGABYTES.toKilobytes(2L));
    assertEquals(42L, SizeUnit.MEGABYTES.toMegabytes(42L));
    assertEquals(0L, SizeUnit.MEGABYTES.toGigabytes(4L));
    assertEquals(10L, SizeUnit.MEGABYTES.toGigabytes(10L * 1024L));
    assertEquals(10L, SizeUnit.MEGABYTES.toTerabytes(10L * 1024L * 1024L));
  }

  @Test
  public void testGigabytes() {
    assertEquals(1024L * 1024L * 1024L, SizeUnit.GIGABYTES.toBytes(1L));
    assertEquals(2 * 1024L * 1024L, SizeUnit.GIGABYTES.toKilobytes(2L));
    assertEquals(2048L, SizeUnit.GIGABYTES.toMegabytes(2L));
    assertEquals(42L, SizeUnit.GIGABYTES.toGigabytes(42L));
    assertEquals(0L, SizeUnit.GIGABYTES.toTerabytes(4L));
    assertEquals(10L, SizeUnit.GIGABYTES.toTerabytes(10L * 1024L));
  }

  @Test
  public void testTerabytes() {
    assertEquals(1024L * 1024L * 1024L * 1024L, SizeUnit.TERABYTES.toBytes(1L));
    assertEquals(2 * 1024L * 1024L * 1024L, SizeUnit.TERABYTES.toKilobytes(2L));
    assertEquals(2L * 1024L * 1024L, SizeUnit.TERABYTES.toMegabytes(2L));
    assertEquals(2048L, SizeUnit.TERABYTES.toGigabytes(2L));
    assertEquals(42L, SizeUnit.TERABYTES.toTerabytes(42L));
  }
}
