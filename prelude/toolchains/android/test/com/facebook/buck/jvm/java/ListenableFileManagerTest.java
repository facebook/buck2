/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNull;

import com.google.common.jimfs.Configuration;
import com.google.common.jimfs.Jimfs;
import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import javax.tools.FileObject;
import javax.tools.JavaFileObject;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

public class ListenableFileManagerTest {

  private static final FileSystem UNIX_FILE_SYSTEM = Jimfs.newFileSystem(Configuration.unix());
  private static final String[] FILE_NAMES = {
    "A", "B", "C", "D", "E", "F", "NonJava", "OTHER", "SOURCE", "HTML"
  };
  private static final Path TEST_JAR_PATH = UNIX_FILE_SYSTEM.getPath("/test.jar");
  private static final String SINGLE_FILE_NAME = "C";
  private static final String SINGLE_NON_JAVA_FILE_NAME = "NonJava";

  private ListenableFileManager fileManager;
  private FakeStandardJavaFileManager fakeFileManager;
  private final List<FileObject> filesRead = new ArrayList<>();
  private final List<FileObject> filesWritten = new ArrayList<>();

  @Before
  public void setUp() {
    fakeFileManager = new FakeStandardJavaFileManager();
    fileManager = new ListenableFileManager(fakeFileManager);
    fileManager.addListener(
        new FileManagerListener() {
          @Override
          public void onFileRead(FileObject file) {
            filesRead.add(file);
          }

          @Override
          public void onFileWritten(FileObject file) {
            filesWritten.add(file);
          }
        });

    for (String fileName : FILE_NAMES) {
      fakeFileManager.addFile(TEST_JAR_PATH, fileName, JavaFileObject.Kind.CLASS);
    }
  }

  @Test
  public void testGetNonExistentJavaFileReturnsNull() throws IOException {
    assertNull(fileManager.getJavaFileForInput(null, "InvalidClass", JavaFileObject.Kind.CLASS));
  }

  @Test
  public void testOpenInputStreamReportsRead() throws IOException {
    fileManager
        .getJavaFileForInput(null, SINGLE_FILE_NAME, JavaFileObject.Kind.CLASS)
        .openInputStream();

    assertFilesRead("C");
    assertNoFilesWritten();
  }

  @Test
  public void testOpenReaderReportsRead() throws IOException {
    fileManager
        .getJavaFileForInput(null, SINGLE_FILE_NAME, JavaFileObject.Kind.CLASS)
        .openReader(true);

    assertFilesRead("C");
    assertNoFilesWritten();
  }

  @Test
  public void testGetCharContentReportsRead() throws IOException {
    fileManager
        .getJavaFileForInput(null, SINGLE_FILE_NAME, JavaFileObject.Kind.CLASS)
        .getCharContent(true);

    assertFilesRead("C");
    assertNoFilesWritten();
  }

  @Test
  public void testOpenOutputStreamReportsWrite() throws IOException {
    fileManager
        .getJavaFileForOutput(null, SINGLE_FILE_NAME, JavaFileObject.Kind.CLASS, null)
        .openOutputStream();

    assertNoFilesRead();
    assertFilesWritten("C");
  }

  @Test
  public void testOpenWriterReportsWrite() throws IOException {
    fileManager
        .getJavaFileForOutput(null, SINGLE_FILE_NAME, JavaFileObject.Kind.CLASS, null)
        .openWriter();

    assertNoFilesRead();
    assertFilesWritten("C");
  }

  private void assertFilesRead(String... fileNames) {
    assertThat(fileNames(filesRead), Matchers.arrayContaining(fileNames));
  }

  private void assertNoFilesRead() {
    assertThat(fileNames(filesRead), Matchers.emptyArray());
  }

  private void assertFilesWritten(String... fileNames) {
    assertThat(fileNames(filesWritten), Matchers.arrayContaining(fileNames));
  }

  private void assertNoFilesWritten() {
    assertThat(fileNames(filesWritten), Matchers.emptyArray());
  }

  private String[] fileNames(List<FileObject> files) {
    return files.stream().map(FileObject::getName).toArray(String[]::new);
  }
}
