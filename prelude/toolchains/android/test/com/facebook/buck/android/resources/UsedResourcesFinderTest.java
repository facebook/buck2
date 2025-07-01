/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.resources;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.testutil.integration.TestDataHelper;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.io.ByteStreams;
import java.io.IOException;
import java.nio.file.Path;
import java.util.zip.ZipFile;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class UsedResourcesFinderTest {
  private static final String APK_NAME = "example.apk";

  @Rule public TemporaryPaths tmpFolder = new TemporaryPaths();
  private Path apkPath;

  @Before
  public void setUp() {
    apkPath = TestDataHelper.getTestDataDirectory(this).resolve("aapt_dump").resolve(APK_NAME);
  }

  @Test
  public void testEmptyRoots() throws IOException {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      UsedResourcesFinder.ResourceClosure closure =
          UsedResourcesFinder.computeClosure(
              new SimpleApkContentProvider(apkZip), ImmutableList.of(), ImmutableList.of());

      assertEquals(ImmutableSet.of(), closure.files);
      assertEquals(ImmutableMap.of(), closure.idsByType);
    }
  }

  @Test
  public void testStringRoots() throws IOException {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      // 0x7f04xxxx are string resources. These don't reference anything else.
      UsedResourcesFinder.ResourceClosure closure =
          UsedResourcesFinder.computeClosure(
              new SimpleApkContentProvider(apkZip),
              ImmutableList.of(),
              ImmutableList.of(0x7f040000, 0x7f040001, 0x7f040002, 0x7f040003));

      assertEquals(ImmutableSet.of(), closure.files);
      assertEquals(ImmutableMap.of(4, ImmutableSet.of(0, 1, 2, 3)), closure.idsByType);
    }
  }

  @Test
  public void testArrayRoot() throws IOException {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      UsedResourcesFinder.ResourceClosure closure =
          UsedResourcesFinder.computeClosure(
              new SimpleApkContentProvider(apkZip),
              ImmutableList.of(),
              ImmutableList.of(0x7f050002));

      assertEquals(ImmutableSet.of(), closure.files);
      assertEquals(
          ImmutableMap.of(5, ImmutableSet.of(2), 6, ImmutableSet.of(1)), closure.idsByType);
    }
  }

  @Test
  public void testXmlRoots() throws IOException {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      UsedResourcesFinder.ResourceClosure closure =
          UsedResourcesFinder.computeClosure(
              new SimpleApkContentProvider(apkZip),
              ImmutableList.of("res/xml/meta_xml.xml"),
              ImmutableList.of());

      assertEquals(
          ImmutableSet.of("res/drawable-nodpi-v4/exo_icon.png", "res/xml/meta_xml.xml"),
          closure.files);
      assertEquals(
          ImmutableMap.of(2, ImmutableSet.of(1), 4, ImmutableSet.of(1)), closure.idsByType);
    }
  }

  @Test
  public void testXmlRootById() throws IOException {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      // Includes a string id so that we process string ids both before and after processing the
      // xml.
      UsedResourcesFinder.ResourceClosure closure =
          UsedResourcesFinder.computeClosure(
              new SimpleApkContentProvider(apkZip),
              ImmutableList.of(),
              ImmutableList.of(0x7f030000, 0x7f040000));

      assertEquals(
          ImmutableSet.of("res/drawable-nodpi-v4/exo_icon.png", "res/xml/meta_xml.xml"),
          closure.files);
      assertEquals(
          ImmutableMap.of(2, ImmutableSet.of(1), 3, ImmutableSet.of(0), 4, ImmutableSet.of(0, 1)),
          closure.idsByType);
    }
  }

  @Test
  public void testPrimaryApkClosure() throws IOException {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      UsedResourcesFinder.ResourceClosure closure =
          UsedResourcesFinder.computePrimaryApkClosure(new SimpleApkContentProvider(apkZip));

      assertEquals(
          ImmutableSet.of(
              "AndroidManifest.xml", "res/drawable-nodpi-v4/exo_icon.png", "res/xml/meta_xml.xml"),
          closure.files);
      assertEquals(
          ImmutableMap.of(
              2, ImmutableSet.of(1),
              3, ImmutableSet.of(0),
              4, ImmutableSet.of(1),
              5, ImmutableSet.of(1, 2),
              6, ImmutableSet.of(1)),
          closure.idsByType);
    }
  }

  private static class SimpleApkContentProvider implements UsedResourcesFinder.ApkContentProvider {
    private final ZipFile apkZip;

    private SimpleApkContentProvider(ZipFile apkZip) {
      this.apkZip = apkZip;
    }

    @Override
    public ResourceTable getResourceTable() {
      return ResourceTable.get(ResChunk.wrap(getContent("resources.arsc")));
    }

    @Override
    public ResourcesXml getXml(String path) {
      return ResourcesXml.get(ResChunk.wrap(getContent(path)));
    }

    private byte[] getContent(String path) {
      try {
        return ByteStreams.toByteArray(apkZip.getInputStream(apkZip.getEntry(path)));
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    @Override
    public boolean hasFile(String path) {
      return apkZip.getEntry(path) != null;
    }
  }
}
