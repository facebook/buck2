/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import static com.facebook.buck.jvm.java.CompilerOutputPaths.getKAPTDepFilePath;
import static com.facebook.buck.jvm.java.CompilerOutputPaths.getKotlinTempDepFilePath;
import static org.junit.Assert.assertEquals;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.util.environment.Platform;
import com.facebook.buck.util.json.ObjectMappers;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

/** Test {@link KotlinClassUsageHelper} */
public class KotlinClassUsageHelperTest {

  public static final String FOO_TEST_FILE_NAME = "Foo.class";
  public static final String BAR_TEST_FILE_NAME = "Bar.class";
  private static final String TEST_JAR_URI =
      Platform.detect() == Platform.WINDOWS ? "/C:/test.jar" : "/test.jar";
  private static final Path TEST_JAR_PATH = Paths.get(URI.create("file://" + TEST_JAR_URI));
  @Rule public TemporaryPaths tmp = new TemporaryPaths();
  private RelPath outputDir;

  @Before
  public void setUp() throws IOException {
    tmp.newFolder("dummyOutputDir");
    outputDir = RelPath.get("dummyOutputDir");
  }

  @Test
  public void testReadJsonDepFile() throws IOException {
    AbsPath kotlinTempDepPath =
        generateDummyKotlinTempFile(
            "dummy.json", ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(FOO_TEST_FILE_NAME), 1)));

    assertEquals(
        ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(FOO_TEST_FILE_NAME), 1)),
        KotlinClassUsageHelper.readJsonBasedClassUsageReport(kotlinTempDepPath.getPath()));
  }

  @Test
  public void testReadNdJsonDepFile() throws IOException {
    AbsPath kotlinTempDepPath =
        generateDummyKotlinTempFile(
            "dummy.json",
            ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(FOO_TEST_FILE_NAME), 1)),
            ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(BAR_TEST_FILE_NAME), 1)));

    assertEquals(
        ImmutableMap.of(
            TEST_JAR_PATH,
            Map.of(Paths.get(FOO_TEST_FILE_NAME), 1, Paths.get(BAR_TEST_FILE_NAME), 1)),
        com.facebook.buck.jvm.kotlin.KotlinClassUsageHelper.readNdJsonBasedClassUsageReport(
            kotlinTempDepPath.getPath()));
  }

  @Test
  public void testReadURIListDepFile() throws IOException {
    AbsPath kaptTempDepPath =
        generateDummyKaptTempFile(
            "dummy.txt",
            ImmutableList.of(
                getURI(TEST_JAR_URI, FOO_TEST_FILE_NAME),
                getURI(TEST_JAR_URI, FOO_TEST_FILE_NAME),
                getURI(TEST_JAR_URI, BAR_TEST_FILE_NAME)));

    assertEquals(
        ImmutableMap.of(
            TEST_JAR_PATH,
            Map.of(Paths.get(FOO_TEST_FILE_NAME), 2, Paths.get(BAR_TEST_FILE_NAME), 1)),
        KotlinClassUsageHelper.readUriBasedClassUsageFile(kaptTempDepPath.getPath()));
  }

  @Test
  public void testMergeSameEntry() {
    assertEquals(
        ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(FOO_TEST_FILE_NAME), 2)),
        KotlinClassUsageHelper.merge(
            ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(FOO_TEST_FILE_NAME), 1)),
            ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(FOO_TEST_FILE_NAME), 1))));
  }

  @Test
  public void testMergeSameAndDifferentEntries() {
    assertEquals(
        ImmutableMap.of(
            TEST_JAR_PATH,
            Map.of(Paths.get(FOO_TEST_FILE_NAME), 2, Paths.get(BAR_TEST_FILE_NAME), 1)),
        KotlinClassUsageHelper.merge(
            ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(FOO_TEST_FILE_NAME), 1)),
            ImmutableMap.of(
                TEST_JAR_PATH,
                Map.of(Paths.get(FOO_TEST_FILE_NAME), 1, Paths.get(BAR_TEST_FILE_NAME), 1))));
  }

  @Test
  public void testReadAllKotlinTempDepFiles() throws IOException {
    generateDummyKotlinTempFile(
        getKotlinTempDepFilePath(outputDir).toString(),
        ImmutableMap.of(TEST_JAR_PATH, Map.of(Paths.get(FOO_TEST_FILE_NAME), 1)));
    generateDummyKaptTempFile(
        getKAPTDepFilePath(outputDir).toString(),
        ImmutableList.of(
            getURI(TEST_JAR_URI, FOO_TEST_FILE_NAME),
            getURI(TEST_JAR_URI, FOO_TEST_FILE_NAME),
            getURI(TEST_JAR_URI, BAR_TEST_FILE_NAME)));

    assertEquals(
        ImmutableMap.of(
            TEST_JAR_PATH,
            Map.of(Paths.get(FOO_TEST_FILE_NAME), 3, Paths.get(BAR_TEST_FILE_NAME), 1)),
        KotlinClassUsageHelper.getClassUsageData(outputDir, tmp.getRoot()));
  }

  private AbsPath generateDummyKotlinTempFile(
      String fileName, ImmutableMap<Path, Map<Path, Integer>>... dummyClassUsagesJson)
      throws IOException {
    AbsPath kotlinTempFile = tmp.newFile(fileName);

    try (FileWriter writer = new FileWriter(kotlinTempFile.toFile(), true)) {
      for (ImmutableMap<Path, Map<Path, Integer>> dummyClassUsageJson : dummyClassUsagesJson) {
        String json = ObjectMappers.WRITER.writeValueAsString(dummyClassUsageJson);
        writer.write(json);
        writer.write("\n");
      }
    }

    return kotlinTempFile;
  }

  private AbsPath generateDummyKaptTempFile(String fileName, ImmutableList<String> uriList)
      throws IOException {
    AbsPath kaptTempDepPath = tmp.newFile(fileName);
    Files.write(kaptTempDepPath.getPath(), uriList);
    return kaptTempDepPath;
  }

  private String getURI(String jarUri, String fileName) {
    return String.format("jar:file://%s!/%s", jarUri, Paths.get(fileName));
  }
}
