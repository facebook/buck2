/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.google.common.collect.FluentIterable;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.tools.JavaFileObject;
import org.junit.Rule;
import org.junit.Test;

public class MergingClassUsageFileWriterTest {

  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  private static final String OTHER_FILE_NAME = JavaFileObject.Kind.OTHER.toString();
  private static final String SOURCE_FILE_NAME = JavaFileObject.Kind.SOURCE.toString();
  private static final String HTML_FILE_NAME = JavaFileObject.Kind.HTML.toString();

  private static final String[] FILE_NAMES = {"A", "B", "C", "D", "E", "F"};
  private static final String SINGLE_NON_JAVA_FILE_NAME = "NonJava";

  @Test
  public void fileReadOrderDoesntAffectClassesUsedOutput() throws IOException {
    RelPath configuredBuckOut = RelPath.get("buck-out/v2");
    AbsPath testJarPath = tmp.getRoot().resolve("test.jar");
    AbsPath testTwoJarPath = tmp.getRoot().resolve("test2.jar");

    AbsPath outputOne = tmp.getRoot().resolve("used-classes-one.json");
    populateFileWithDefaultJson(outputOne.getPath());
    AbsPath outputTwo = tmp.getRoot().resolve("used-classes-two.json");
    populateFileWithDefaultJson(outputTwo.getPath());

    FakeStandardJavaFileManager fakeFileManager = new FakeStandardJavaFileManager();
    fakeFileManager.addFile(testJarPath, OTHER_FILE_NAME, JavaFileObject.Kind.OTHER);
    fakeFileManager.addFile(testJarPath, SOURCE_FILE_NAME, JavaFileObject.Kind.SOURCE);
    fakeFileManager.addFile(testJarPath, HTML_FILE_NAME, JavaFileObject.Kind.HTML);
    fakeFileManager.addFile(testJarPath, SINGLE_NON_JAVA_FILE_NAME, JavaFileObject.Kind.OTHER);
    for (String fileName : FILE_NAMES) {
      fakeFileManager.addFile(testJarPath, fileName, JavaFileObject.Kind.CLASS);
      fakeFileManager.addFile(testTwoJarPath, fileName, JavaFileObject.Kind.CLASS);
    }

    MergingClassUsageFileWriter writerOne = new MergingClassUsageFileWriter(outputOne);
    ClassUsageTracker trackerOne = new ClassUsageTracker();
    {
      ListenableFileManager wrappedFileManager = new ListenableFileManager(fakeFileManager);
      wrappedFileManager.addListener(trackerOne);
      for (JavaFileObject javaFileObject : wrappedFileManager.list(null, null, null, false)) {
        javaFileObject.openInputStream();
      }
    }
    Files.createDirectories(outputOne.getParent().getPath());
    writerOne.writeFile(
        trackerOne.getClassUsageMap(),
        tmp.getRoot().relativize(outputOne),
        tmp.getRoot(),
        configuredBuckOut);

    MergingClassUsageFileWriter writerTwo = new MergingClassUsageFileWriter(outputTwo);
    ClassUsageTracker trackerTwo = new ClassUsageTracker();
    {
      ListenableFileManager wrappedFileManager = new ListenableFileManager(fakeFileManager);
      wrappedFileManager.addListener(trackerTwo);
      Iterable<JavaFileObject> fileObjects = wrappedFileManager.list(null, null, null, false);
      for (JavaFileObject javaFileObject : FluentIterable.from(fileObjects).toList().reverse()) {
        javaFileObject.openInputStream();
      }
    }
    Files.createDirectories(outputTwo.getParent().getPath());
    writerTwo.writeFile(
        trackerTwo.getClassUsageMap(),
        tmp.getRoot().relativize(outputTwo),
        tmp.getRoot(),
        configuredBuckOut);

    assertEquals(
        new String(Files.readAllBytes(outputOne.getPath())),
        new String(Files.readAllBytes(outputTwo.getPath())));
  }

  private static void populateFileWithDefaultJson(Path path) throws IOException {
    String jsonContent = "{" + getDefaultJsonContent() + "}";

    try (Writer writer = Files.newBufferedWriter(path)) {
      writer.write(jsonContent);
    }
  }

  private static String getDefaultJsonContent() {
    return "\"default.jar\":{\"A\":1,\"B\":1,\"C\":1,\"D\":1,\"E\":1,\"F\":1,\"HTML\":1,\"NonJava\":1,\"OTHER\":1,\"SOURCE\":1},"
               + "\"test.jar\":{\"A\":1,\"B\":1,\"C\":1,\"D\":1,\"E\":1,\"F\":1}";
  }
}
