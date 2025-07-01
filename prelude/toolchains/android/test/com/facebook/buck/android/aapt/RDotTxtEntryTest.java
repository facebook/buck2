/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.aapt;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.android.aapt.RDotTxtEntry.IdType;
import com.facebook.buck.android.aapt.RDotTxtEntry.RType;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Ordering;
import org.junit.Test;

public class RDotTxtEntryTest {

  @Test
  public void testRDotTxtEntryCompareTo() {
    ImmutableList<RDotTxtEntry> entries =
        ImmutableList.of(
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "ActionBar", null),
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "ActionBarLayout", "0x7f060008"),
            new RDotTxtEntry(IdType.INT, RType.STYLEABLE, "ActionBar_background", "2", "ActionBar"),
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "ActionBar_contentInsetEnd", "0", "ActionBar"),
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "ActionBarLayout_android", "0", "ActionBarLayout"),
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "ActionBar_backgroundStack", "1", "ActionBar"));

    ImmutableList<RDotTxtEntry> sortedEntries =
        ImmutableList.copyOf(Ordering.natural().sortedCopy(entries));

    assertEquals(
        ImmutableList.of(
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "ActionBar", null),
            new RDotTxtEntry(IdType.INT, RType.STYLEABLE, "ActionBar_background", "2", "ActionBar"),
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "ActionBar_backgroundStack", "1", "ActionBar"),
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "ActionBar_contentInsetEnd", "0", "ActionBar"),
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "ActionBarLayout", "0x7f060008"),
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "ActionBarLayout_android", "0", "ActionBarLayout")),
        sortedEntries);
  }
}
