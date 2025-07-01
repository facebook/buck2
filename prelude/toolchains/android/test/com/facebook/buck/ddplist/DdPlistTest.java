/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.ddplist;

import static org.junit.Assert.assertEquals;

import com.dd.plist.NSDictionary;
import com.dd.plist.PropertyListParser;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import org.junit.Before;
import org.junit.Test;

public class DdPlistTest {

  private File outputFile;

  @Before
  public void setUp() throws IOException {
    outputFile = File.createTempFile("out", ".plist");
  }

  @Test
  public void testXMLWriting() throws Exception {
    InputStream in = getClass().getResourceAsStream("test-files/test1.plist");
    NSDictionary x = (NSDictionary) PropertyListParser.parse(in);
    PropertyListParser.saveAsXML(x, outputFile);
    NSDictionary y = (NSDictionary) PropertyListParser.parse(outputFile);
    assertEquals(x, y);
  }
}
