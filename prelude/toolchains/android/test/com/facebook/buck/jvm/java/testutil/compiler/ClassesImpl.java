/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.testutil.compiler;

import com.facebook.buck.io.pathformat.PathFormatter;
import com.facebook.buck.util.zip.CustomZipEntry;
import com.facebook.buck.util.zip.JarBuilder;
import com.facebook.buck.util.zip.JarEntrySupplier;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import org.junit.rules.TemporaryFolder;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;

public class ClassesImpl implements Classes {
  private final TemporaryFolder root;

  public ClassesImpl(TemporaryFolder root) {
    this.root = root;
  }

  @Override
  public void acceptClassVisitor(String qualifiedName, int flags, ClassVisitor cv)
      throws IOException {
    Path classFilePath = resolveClassFilePath(qualifiedName);

    try (InputStream stream = Files.newInputStream(classFilePath)) {
      ClassReader reader = new ClassReader(stream);

      reader.accept(cv, flags);
    }
  }

  @Override
  public void createJar(Path jarPath, boolean hashEntries) throws IOException {
    JarBuilder jarBuilder =
        new JarBuilder().setShouldMergeManifests(true).setShouldHashEntries(hashEntries);

    writeToJar(jarBuilder);

    jarBuilder.createJarFile(jarPath);
  }

  @Override
  public void writeToJar(JarBuilder jarBuilder) throws IOException {
    List<Path> files =
        Files.walk(root.getRoot().toPath())
            .filter(path -> path.toFile().isFile())
            .sorted()
            .collect(Collectors.toList());

    for (Path file : files) {
      jarBuilder.addEntry(
          new JarEntrySupplier(
              new CustomZipEntry(
                  PathFormatter.pathWithUnixSeparators(root.getRoot().toPath().relativize(file))),
              () -> Files.newInputStream(file)));
    }
  }

  private Path resolveClassFilePath(String qualifiedName) {
    return root.getRoot()
        .toPath()
        .resolve(qualifiedName.replace('.', File.separatorChar) + ".class");
  }
}
