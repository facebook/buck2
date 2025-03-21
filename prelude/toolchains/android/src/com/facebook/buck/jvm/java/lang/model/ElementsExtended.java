/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.lang.model;

import com.facebook.buck.util.liteinfersupport.Nullable;
import java.util.List;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;

/**
 * Wraps and extends {@link javax.lang.model.util.Elements} with methods that cannot be added as
 * pure extension methods on {@link MoreElements} because they require per-instance state.
 */
public interface ElementsExtended extends Elements {

  List<ExecutableElement> getDeclaredMethods(TypeElement owner, CharSequence name);

  List<ExecutableElement> getAllMethods(TypeElement owner, CharSequence name);

  List<BridgeMethod> getBridgeMethods(TypeElement owner, CharSequence name);

  List<BridgeMethod> getAllBridgeMethods(TypeElement type);

  @Nullable
  ExecutableElement getImplementation(ExecutableElement method, TypeElement inType);

  @Nullable
  TypeElement getBinaryImplementationOwner(ExecutableElement method, TypeElement inType);

  boolean isCompiledInCurrentRun(Element element);

  /**
   * Return type annotations present on an element.
   *
   * <p>Examples:
   *
   * <ul>
   *   <li>For methods this means type annotations present on method's type parameters and types of
   *       formal parameters.
   *   <li>For classes it's type annotations on a class and its superclasses and superinterfaces.
   *   <li>For fields it's type annotations on a field's type.
   * </ul>
   */
  List<? extends AnnotationMirror> getAllTypeAnnotations(Element element);
}
