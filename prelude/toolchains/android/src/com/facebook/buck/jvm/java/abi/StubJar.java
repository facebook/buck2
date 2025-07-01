/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.java.abi.kotlin.InlineFunctionScope;
import com.facebook.buck.jvm.java.lang.model.ElementsExtended;
import com.facebook.buck.util.zip.JarBuilder;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import javax.annotation.Nullable;
import javax.annotation.processing.Messager;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.util.Types;

public class StubJar {
  private final Supplier<LibraryReader> libraryReaderSupplier;
  @Nullable private AbiGenerationMode compatibilityMode = null;
  @Nullable private Supplier<LibraryReader> existingAbiSupplier = null;
  private boolean keepSynthetic = false;

  public StubJar(AbsPath jarPath) {
    libraryReaderSupplier = () -> LibraryReader.of(jarPath.getPath());
  }

  public StubJar(AbsPath jarPath, boolean keepSynthetic) {
    libraryReaderSupplier = () -> LibraryReader.of(jarPath.getPath());
    this.keepSynthetic = keepSynthetic;
  }

  /**
   * @param targetVersion the class file version to output, expressed as the corresponding Java
   *     source version
   */
  public StubJar(
      SourceVersion targetVersion,
      ElementsExtended elements,
      Types types,
      Messager messager,
      Iterable<Element> topLevelElements,
      boolean includeParameterMetadata,
      boolean keepSynthetic) {
    libraryReaderSupplier =
        () ->
            LibraryReader.of(
                targetVersion,
                elements,
                types,
                messager,
                topLevelElements,
                includeParameterMetadata);
  }

  /**
   * Filters the stub jar through {@link SourceAbiCompatibleVisitor}. See that class for details.
   */
  public StubJar setCompatibilityMode(AbiGenerationMode compatibilityMode) {
    this.compatibilityMode = compatibilityMode;
    return this;
  }

  /** Specify a directory of existing abi we need to inherit */
  public StubJar setExistingAbiJar(AbsPath existingAbiJar) {
    this.existingAbiSupplier = () -> LibraryReader.of(existingAbiJar.getPath());
    return this;
  }

  /** Writes output into the passed absolute path. */
  public void writeTo(AbsPath outputAbsPath) throws IOException {
    // The order of these declarations is important -- FilesystemStubJarWriter must be declared
    // after LibraryReader. This is because FilesystemStubJarWriter actually uses the LibraryReader
    // in its close method, and try-with-resources closes the items in the opposite order of their
    // creation.
    try (LibraryReader input = libraryReaderSupplier.get();
        LibraryReader existingAbiInput =
            existingAbiSupplier != null ? existingAbiSupplier.get() : null;
        StubJarWriter writer = new FilesystemStubJarWriter(outputAbsPath)) {
      writeTo(input, writer, existingAbiInput);
    }
  }

  public void writeTo(JarBuilder jarBuilder) throws IOException {
    try (LibraryReader input = libraryReaderSupplier.get();
        LibraryReader existingAbiInput =
            existingAbiSupplier != null ? existingAbiSupplier.get() : null;
        StubJarWriter writer = new JarBuilderStubJarWriter(jarBuilder)) {
      writeTo(input, writer, existingAbiInput);
    }
  }

  private void writeTo(
      LibraryReader input, StubJarWriter writer, @Nullable LibraryReader existingAbiInput)
      throws IOException {
    List<Path> relativePaths = input.getRelativePaths();
    Comparator<Path> visitOuterClassesFirst = Comparator.comparing(StubJar::pathWithoutClassSuffix);
    List<Path> paths =
        relativePaths.stream().sorted(visitOuterClassesFirst).collect(Collectors.toList());

    boolean isKotlinModule = isKotlinModule(relativePaths);

    InlineFunctionScope inlineFunctionScope = isKotlinModule ? new InlineFunctionScope() : null;

    if (!isKotlinModule && keepSynthetic) {
      throw new IllegalStateException(
          "keepSynthetic is intended only to be used on Kotlin modules");
    }

    // Calculate all existingAbiPathStrings here outside of for-loop,
    // to save calculation for each path / StubJarEntry
    Set<String> existingAbiPathStrings =
        existingAbiInput != null
            ? existingAbiInput.getRelativePaths().stream()
                .map(Path::toString)
                .collect(Collectors.toSet())
            : Collections.emptySet();

    for (Path path : paths) {
      StubJarEntry entry =
          StubJarEntry.of(
              input,
              path,
              existingAbiInput,
              existingAbiPathStrings,
              compatibilityMode,
              inlineFunctionScope,
              keepSynthetic);
      if (entry == null) {
        continue;
      }
      entry.write(writer);
      if (inlineFunctionScope != null) {
        String pathNoSuffix = pathWithoutClassSuffix(path);
        inlineFunctionScope.createScopes(pathNoSuffix, entry.getInlineFunctions());
        if (entry.extendsInlineFunctionScope()) {
          inlineFunctionScope.extendScope(pathNoSuffix);
        }
      }
    }
  }

  private boolean isKotlinModule(List<Path> relativePaths) {
    return relativePaths.stream().anyMatch(path -> path.toString().endsWith(".kotlin_module"));
  }

  static String pathWithoutClassSuffix(Path path) {
    final String pathString = path.toString();
    return pathString.endsWith(".class")
        ? pathString.substring(0, pathString.length() - ".class".length())
        : pathString;
  }
}
