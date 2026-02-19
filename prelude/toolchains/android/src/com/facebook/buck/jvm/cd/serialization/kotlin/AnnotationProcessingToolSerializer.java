/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.kotlin;

import com.facebook.buck.jvm.cd.command.kotlin.AnnotationProcessingTool;
import com.facebook.infer.annotation.Nullsafe;

/**
 * Marshalling between:
 *
 * <ul>
 *   <li>{@link com.facebook.buck.jvm.cd.command.kotlin.AnnotationProcessingTool}, and
 *   <li>{@link com.facebook.buck.cd.model.kotlin.AnnotationProcessingTool} (part of the protocol
 *       buffer model).
 * </ul>
 */
@Nullsafe(Nullsafe.Mode.LOCAL)
public class AnnotationProcessingToolSerializer {

  private AnnotationProcessingToolSerializer() {}

  /** Protocol buffer model to internal buck representation. */
  public static AnnotationProcessingTool deserialize(
      com.facebook.buck.cd.model.kotlin.AnnotationProcessingTool annotationProcessingTool) {
    switch (annotationProcessingTool) {
      case KAPT:
        return AnnotationProcessingTool.KAPT;

      case JAVAC:
        return AnnotationProcessingTool.JAVAC;

      case UNRECOGNIZED:
      default:
        throw new IllegalArgumentException(
            "Unrecognised annotation processing tool: " + annotationProcessingTool);
    }
  }
}
