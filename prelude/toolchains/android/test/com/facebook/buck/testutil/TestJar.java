/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testutil;

import com.google.common.io.ByteStreams;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarFile;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

public class TestJar implements Closeable {
  private final JarFile jarFile;

  public TestJar(File file) throws IOException {
    jarFile = new JarFile(file);
  }

  @Override
  public void close() throws IOException {
    jarFile.close();
  }

  public List<? extends ZipEntry> getZipEntries() {
    return jarFile.stream().collect(Collectors.toList());
  }

  public List<String> getEntriesContent() {
    List<String> result = new ArrayList<>();
    for (ZipEntry zipEntry : getZipEntries()) {
      try (InputStream entryStream = jarFile.getInputStream(zipEntry)) {
        result.add(new String(ByteStreams.toByteArray(entryStream), StandardCharsets.UTF_8));
      } catch (IOException e) {
        throw new AssertionError(e);
      }
    }
    return result;
  }
}
