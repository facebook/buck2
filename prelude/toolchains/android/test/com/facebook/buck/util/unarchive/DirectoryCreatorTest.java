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

import com.facebook.buck.testutil.TemporaryPaths;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class DirectoryCreatorTest {

  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  @Before
  public void setUp() {}

  @Test
  public void mkdirsCreatesDirectoryIfHasntBeenCreated() throws IOException {
    Path path = Paths.get("foo", "bar");
    Path absPath = tmp.getRoot().resolve(path).getPath();
    DirectoryCreator creator = new DirectoryCreator(tmp.getRoot());

    creator.mkdirs(path);

    Assert.assertTrue(Files.exists(absPath));
    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo")));
    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo", "bar")));
  }

  @Test
  public void mkdirsDoesNotCreateDirectoryIfAlreadyCreated() throws IOException {
    Path path = Paths.get("foo", "bar");
    Path absPath = tmp.getRoot().resolve(path).getPath();
    DirectoryCreator creator = new DirectoryCreator(tmp.getRoot());

    creator.mkdirs(path);
    Files.delete(absPath);
    Assert.assertFalse(Files.exists(absPath));
    creator.mkdirs(path);

    Assert.assertFalse(Files.exists(absPath));
    // Recorded on first creation
    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo")));
    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo", "bar")));
  }

  @Test
  public void mkdirsRecordsParentDirectoriesIfChildDoesNotExistButParentDoes() throws IOException {
    Path path = Paths.get("foo", "bar");
    Path absPath = tmp.getRoot().resolve(path).getPath();
    DirectoryCreator creator = new DirectoryCreator(tmp.getRoot());

    Files.createDirectories(absPath.getParent());
    creator.mkdirs(path);

    Assert.assertTrue(Files.exists(absPath));
    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo")));
    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo", "bar")));
  }

  @Test
  public void recordPathRecordsProperly() {
    Path path = Paths.get("foo", "bar");
    DirectoryCreator creator = new DirectoryCreator(tmp.getRoot());

    creator.recordPath(path);

    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo", "bar")));
  }

  @Test
  public void forcefullyCreateDirsDoesNothingIfDirectoryAlreadyExists() throws IOException {
    Path path = Paths.get("foo", "bar");
    Path absPath = tmp.getRoot().resolve(path).getPath();
    DirectoryCreator creator = new DirectoryCreator(tmp.getRoot());

    Files.createDirectories(absPath);
    Files.writeString(absPath.resolve("subfile"), "test");
    creator.forcefullyCreateDirs(path);

    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo", "bar")));
    Assert.assertTrue(Files.exists(absPath.resolve("subfile")));
  }

  @Test
  public void forcefullyCreateDirsDeletesFileAndCreatesDirAlreadyExists() throws IOException {
    Path path = Paths.get("foo", "bar");
    Path absPath = tmp.getRoot().resolve(path).getPath();
    DirectoryCreator creator = new DirectoryCreator(tmp.getRoot());

    Files.createDirectories(absPath.getParent());
    Files.writeString(absPath, "test");
    creator.forcefullyCreateDirs(path);

    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo", "bar")));
    Assert.assertTrue(Files.isDirectory(absPath));
  }

  @Test
  public void forcefullyCreateDirsRemovesParentFilesIfParentExistsAndChildDoesNot()
      throws IOException {
    Path path = Paths.get("foo", "bar");
    Path absPath = tmp.getRoot().resolve(path).getPath();
    DirectoryCreator creator = new DirectoryCreator(tmp.getRoot());

    Files.writeString(absPath.getParent(), "test");
    creator.forcefullyCreateDirs(path);

    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo")));
    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo", "bar")));
    Assert.assertTrue(Files.isDirectory(absPath.getParent()));
    Assert.assertTrue(Files.isDirectory(absPath));
  }

  @Test
  public void forcefullyCreateDirsDoesNotCreateAnythingIfAlreadyAdded() throws IOException {
    Path path = Paths.get("foo", "bar");
    Path absPath = tmp.getRoot().resolve(path).getPath();
    DirectoryCreator creator = new DirectoryCreator(tmp.getRoot());

    creator.forcefullyCreateDirs(path);
    Files.delete(absPath);
    Assert.assertFalse(Files.exists(absPath));
    creator.forcefullyCreateDirs(path);

    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo")));
    Assert.assertTrue(creator.recordedDirectories().contains(Paths.get("foo", "bar")));
    Assert.assertFalse(Files.exists(absPath));
  }
}
