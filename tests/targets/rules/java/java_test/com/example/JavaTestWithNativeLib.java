// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.example;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class JavaTestWithNativeLib {

  @Test
  public void testGetNativeValue() {
    assertEquals(JLib.getValue(), 3);
  }
}
