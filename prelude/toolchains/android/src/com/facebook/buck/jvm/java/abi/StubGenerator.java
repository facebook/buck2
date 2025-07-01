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
import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.jvm.java.lang.model.ElementsExtended;
import com.facebook.buck.util.zip.JarBuilder;
import java.io.IOException;
import java.util.Set;
import javax.annotation.processing.Messager;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.util.Types;

public class StubGenerator {
  private final SourceVersion version;
  private final ElementsExtended elements;
  private final Types types;
  private final Messager messager;
  private final JarBuilder jarBuilder;
  private final AbiGenerationMode abiCompatibilityMode;
  private final boolean includeParameterMetadata;
  private final boolean keepSynthetic;

  public StubGenerator(
      SourceVersion version,
      ElementsExtended elements,
      Types types,
      Messager messager,
      JarBuilder jarBuilder,
      AbiGenerationMode abiCompatibilityMode,
      boolean includeParameterMetadata,
      boolean keepSynthetic) {
    this.version = version;
    this.elements = elements;
    this.types = types;
    this.messager = messager;
    this.jarBuilder = jarBuilder;
    this.abiCompatibilityMode = abiCompatibilityMode;
    this.includeParameterMetadata = includeParameterMetadata;
    this.keepSynthetic = keepSynthetic;
  }

  public void generate(Set<Element> topLevelElements) {
    try {
      new StubJar(
              version,
              elements,
              types,
              messager,
              topLevelElements,
              includeParameterMetadata,
              keepSynthetic)
          .setCompatibilityMode(abiCompatibilityMode)
          .writeTo(jarBuilder);
    } catch (IOException e) {
      throw new HumanReadableException("Failed to generate abi: %s", e.getMessage());
    }
  }
}
