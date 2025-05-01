/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.api;

import com.facebook.buck.util.liteinfersupport.Nullable;
import javax.lang.model.element.TypeElement;
import javax.tools.JavaFileObject;

/**
 * {@link com.sun.source.util.TaskEvent} is included with the compiler and is thus not directly
 * accessible from within Buck's class loader. This class is used as a mirror within Buck's class
 * loader for a {@link com.sun.source.util.TaskEvent} within the compiler's.
 */
public final class TaskEventMirror {
  public enum Kind {
    /** For events related to the parsing of a file. */
    PARSE,
    /** For events relating to elements being entered. */
    ENTER,
    /** For events relating to elements being analyzed for errors. */
    ANALYZE,
    /** For events relating to class files being generated. */
    GENERATE,
    /** For events relating to overall annotation processing. */
    ANNOTATION_PROCESSING,
    /** For events relating to an individual annotation processing round. */
    ANNOTATION_PROCESSING_ROUND,
    /**
     * Sent before parsing first source file, and after writing the last output file. This event is
     * not sent when using {@code JavacTask#parse()}, {@code JavacTask#analyze()} or {@code
     * JavacTask#generate()}.
     *
     * @since 9
     */
    COMPILATION,
  }

  private final Object original;
  private final Kind kind;
  @Nullable private final JavaFileObject sourceFile;
  @Nullable private final CompilationUnitTreeProxy compilationUnitTreeProxy;
  @Nullable private final TypeElement typeElement;

  public TaskEventMirror(
      Object original,
      Kind kind,
      @Nullable JavaFileObject sourceFile,
      @Nullable CompilationUnitTreeProxy compilationUnitTreeProxy,
      @Nullable TypeElement typeElement) {
    this.original = original;
    this.kind = kind;
    this.sourceFile = sourceFile;
    this.compilationUnitTreeProxy = compilationUnitTreeProxy;
    this.typeElement = typeElement;
  }

  public Object getOriginal() {
    return original;
  }

  public Kind getKind() {
    return kind;
  }

  @Nullable
  public JavaFileObject getSourceFile() {
    return sourceFile;
  }

  @Nullable
  public CompilationUnitTreeProxy getCompilationUnit() {
    return compilationUnitTreeProxy;
  }

  @Nullable
  public TypeElement getTypeElement() {
    return typeElement;
  }
}
