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
import java.util.Arrays;
import java.util.Collection;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class RDotTxtEntryOrderingTest {

  private final RDotTxtEntry entry1;
  private final RDotTxtEntry entry2;
  private final int result;

  @Parameterized.Parameters
  public static Collection<Object[]> testData() {
    return Arrays.asList(
        new Object[][] {
          {
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "ActionBar_contentInsetEnd", "1", "ActionBar"),
            new RDotTxtEntry(
                IdType.INT,
                RType.STYLEABLE,
                "ActionBar_contentInsetEnd__android",
                "0",
                "ActionBar"),
            -1
          },
          {
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "AnchorLayout", "{ 0x7f020001 }"),
            new RDotTxtEntry(
                IdType.INT_ARRAY,
                RType.STYLEABLE,
                "AnchorLayout_Layout",
                "{ 0x7f020002,0x7f020003 }"),
            -1
          },
          {
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "AnchorLayout", "{ 0x7f020001 }"),
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "AnchorLayout_attr1", "0", "AnchorLayout"),
            -1
          },
          {
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "AnchorLayout", "{ 0x7f020001 }"),
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "BlahLayout", "{ 0x7f020002 }"),
            -1
          },
          {
            new RDotTxtEntry(
                IdType.INT_ARRAY, RType.STYLEABLE, "AnchorLayout_Layout", "{ 0x7f020001 }"),
            new RDotTxtEntry(
                IdType.INT_ARRAY, RType.STYLEABLE, "BlahLayout_Layout", "{ 0x7f020002 }"),
            -1
          },
          {
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "AnchorLayout_attr1", "0", "AnchorLayout"),
            new RDotTxtEntry(
                IdType.INT_ARRAY, RType.STYLEABLE, "BlahLayout_Layout", "{ 0x7f020002 }"),
            -1
          },
          {
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "AnchorLayout_attr1", "0", "AnchorLayout"),
            new RDotTxtEntry(
                IdType.INT_ARRAY,
                RType.STYLEABLE,
                "AnchorLayout_Layout",
                "{ 0x7f020002,0x7f020003 }"),
            -1
          },
          {
            new RDotTxtEntry(IdType.INT, RType.STYLEABLE, "AnchorLayout_z1", "0", "AnchorLayout"),
            new RDotTxtEntry(
                IdType.INT_ARRAY,
                RType.STYLEABLE,
                "AnchorLayout_Layout",
                "{ 0x7f020002,0x7f020003 }"),
            -1
          },
          {
            new RDotTxtEntry(IdType.INT, RType.STYLEABLE, "BlahLayout_attr1", "0", "BlahLayout"),
            new RDotTxtEntry(
                IdType.INT_ARRAY, RType.STYLEABLE, "AnchorLayout_Layout", "{ 0x7f020002 }"),
            1
          },
          {
            new RDotTxtEntry(IdType.INT_ARRAY, RType.STYLEABLE, "BlahLayout", "{ 0x7f020001 }"),
            new RDotTxtEntry(
                IdType.INT_ARRAY, RType.STYLEABLE, "AnchorLayout_Layout", "{ 0x7f020002 }"),
            1
          },
          {
            new RDotTxtEntry(
                IdType.INT, RType.STYLEABLE, "AlertDialog_android_layout", "0", "AlertDialog"),
            new RDotTxtEntry(
                IdType.INT,
                RType.STYLEABLE,
                "AlertDialog_buttonPanelSideLayout",
                "1",
                "AlertDialog"),
            -1
          },
        });
  }

  public RDotTxtEntryOrderingTest(RDotTxtEntry entry1, RDotTxtEntry entry2, int result) {
    this.entry1 = entry1;
    this.entry2 = entry2;
    this.result = result;
  }

  @Test
  public void testOrdering() {
    assertEquals(result, entry1.compareTo(entry2));
  }
}
