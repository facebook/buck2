/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.adapter;

import com.facebook.buck.util.liteinfersupport.Nullable;
import java.io.Writer;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.Name;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;

/** Delegates all method calls to an inner instance of {@link Elements}. */
class DelegatingElements implements Elements {
  private final Elements inner;

  public DelegatingElements(Elements inner) {
    this.inner = inner;
  }

  @Override
  @Nullable
  public PackageElement getPackageElement(CharSequence name) {
    return inner.getPackageElement(name);
  }

  @Override
  @Nullable
  public PackageElement getPackageElement(ModuleElement module, CharSequence name) {
    return inner.getPackageElement(module, name);
  }

  @Override
  public Set<? extends PackageElement> getAllPackageElements(CharSequence name) {
    return inner.getAllPackageElements(name);
  }

  @Override
  @Nullable
  public TypeElement getTypeElement(CharSequence name) {
    return inner.getTypeElement(name);
  }

  @Override
  @Nullable
  public ModuleElement getModuleElement(CharSequence name) {
    return inner.getModuleElement(name);
  }

  @Override
  public Set<? extends ModuleElement> getAllModuleElements() {
    return inner.getAllModuleElements();
  }

  @Override
  public Map<? extends ExecutableElement, ? extends AnnotationValue> getElementValuesWithDefaults(
      AnnotationMirror a) {
    return inner.getElementValuesWithDefaults(a);
  }

  @Override
  @Nullable
  public String getDocComment(Element e) {
    return inner.getDocComment(e);
  }

  @Override
  public boolean isDeprecated(Element e) {
    return inner.isDeprecated(e);
  }

  @Override
  public Name getBinaryName(TypeElement type) {
    return inner.getBinaryName(type);
  }

  @Override
  public PackageElement getPackageOf(Element type) {
    return inner.getPackageOf(type);
  }

  @Override
  public List<? extends Element> getAllMembers(TypeElement type) {
    return inner.getAllMembers(type);
  }

  @Override
  public List<? extends AnnotationMirror> getAllAnnotationMirrors(Element e) {
    return inner.getAllAnnotationMirrors(e);
  }

  @Override
  public boolean hides(Element hider, Element hidden) {
    return inner.hides(hider, hidden);
  }

  @Override
  public boolean overrides(
      ExecutableElement overrider, ExecutableElement overridden, TypeElement type) {
    return inner.overrides(overrider, overridden, type);
  }

  @Override
  public String getConstantExpression(Object value) {
    return inner.getConstantExpression(value);
  }

  @Override
  public void printElements(Writer w, Element... elements) {
    inner.printElements(w, elements);
  }

  @Override
  public Name getName(CharSequence cs) {
    return inner.getName(cs);
  }

  @Override
  public boolean isFunctionalInterface(TypeElement type) {
    return inner.isFunctionalInterface(type);
  }
}
