/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.adapter;

import com.facebook.buck.jvm.java.lang.model.BridgeMethod;
import com.facebook.buck.jvm.java.lang.model.ElementsExtended;
import com.facebook.buck.jvm.java.lang.model.MoreAnnotations;
import com.facebook.buck.jvm.java.lang.model.MoreElements;
import com.facebook.buck.util.liteinfersupport.Nullable;
import com.sun.source.util.Trees;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.ElementFilter;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;

/**
 * Wraps and extends {@link javax.lang.model.util.Elements} with methods that cannot be added as
 * pure extension methods on {@link MoreElements} because they require per-instance state.
 */
public class ElementsExtendedImpl extends DelegatingElements implements ElementsExtended {

  private final Map<TypeElement, Map<Name, List<ExecutableElement>>> declaredMethodsMaps =
      new HashMap<>();
  private final Map<TypeElement, Map<Name, List<ExecutableElement>>> allMethodsMaps =
      new HashMap<>();
  private final BridgeMethods bridgeMethods;
  private final Types types;
  private final Trees trees;

  public ElementsExtendedImpl(Elements inner, Types types, Trees trees) {
    super(inner);
    this.types = types;
    this.trees = trees;

    bridgeMethods = new BridgeMethods(this, types);
  }

  @Override
  public List<ExecutableElement> getDeclaredMethods(TypeElement owner, CharSequence name) {
    return getMethods(owner, name, declaredMethodsMaps, Element::getEnclosedElements);
  }

  @Override
  public List<ExecutableElement> getAllMethods(TypeElement owner, CharSequence name) {
    return getMethods(owner, name, allMethodsMaps, this::getAllMembers);
  }

  @Override
  public List<BridgeMethod> getBridgeMethods(TypeElement owner, CharSequence name) {
    return bridgeMethods.getBridgeMethods(owner, getName(name));
  }

  @Override
  @Nullable
  public ExecutableElement getImplementation(ExecutableElement baseMethod, TypeElement inType) {
    ExecutableElement result = null;
    for (ExecutableElement candidate : getAllMethods(inType, baseMethod.getSimpleName())) {
      Element enclosingElement = candidate.getEnclosingElement();
      if (enclosingElement != inType && enclosingElement.getKind().isInterface()) {
        continue;
      }

      if (overrides(candidate, baseMethod, inType) || (result == null && candidate == baseMethod)) {
        result = candidate;
      }
    }
    return result;
  }

  @Nullable
  @Override
  public TypeElement getBinaryImplementationOwner(ExecutableElement method, TypeElement inType) {
    TypeElement implementationOwner = null;
    ExecutableElement implementation = getImplementation(method, inType);
    if (implementation != null && binarySignaturesMatch(implementation, method)) {
      implementationOwner = (TypeElement) implementation.getEnclosingElement();
    }

    for (TypeElement type = inType; type != null; type = MoreElements.getSuperclass(type)) {
      if (implementationOwner != null
          && !types.isSubtype(
              types.erasure(type.asType()), types.erasure(implementationOwner.asType()))) {
        break;
      }

      Name name = method.getSimpleName();
      List<BridgeMethod> bridgeMethods = this.bridgeMethods.getBridgeMethodsNoCreate(type, name);

      Optional<ExecutableElement> result =
          bridgeMethods.stream()
              .map(it -> it.to)
              .filter(it -> binarySignaturesMatch(it, method))
              .findFirst();
      if (result.isPresent()) {
        implementationOwner = type;
        break;
      }
    }

    return implementationOwner;
  }

  private boolean binarySignaturesMatch(ExecutableElement e1, ExecutableElement e2) {
    return types.isSameType(types.erasure(e1.asType()), types.erasure(e2.asType()))
        && types.isSameType(types.erasure(e1.getReturnType()), types.erasure(e2.getReturnType()));
  }

  @Override
  public List<BridgeMethod> getAllBridgeMethods(TypeElement type) {
    return bridgeMethods.getBridgeMethods(type);
  }

  private List<ExecutableElement> getMethods(
      TypeElement owner,
      CharSequence name,
      Map<TypeElement, Map<Name, List<ExecutableElement>>> methodsMaps,
      Function<? super TypeElement, List<? extends Element>> getMembersFn) {
    Map<Name, List<ExecutableElement>> methodsMap =
        methodsMaps.computeIfAbsent(owner, el -> buildMethodsMap(el, getMembersFn));

    List<ExecutableElement> result = methodsMap.get(getName(name));
    if (result == null) {
      result = Collections.emptyList();
    }
    return result;
  }

  @Override
  public boolean isCompiledInCurrentRun(Element element) {
    return trees.getTree(element) != null;
  }

  @Override
  public List<? extends AnnotationMirror> getAllTypeAnnotations(Element element) {
    return MoreAnnotations.getAllTypeAnnotations(element);
  }

  private static Map<Name, List<ExecutableElement>> buildMethodsMap(
      TypeElement owner, Function<? super TypeElement, List<? extends Element>> getMembersFn) {
    Map<Name, List<ExecutableElement>> result = new HashMap<>();

    for (ExecutableElement method : ElementFilter.methodsIn(getMembersFn.apply(owner))) {
      List<ExecutableElement> methodsWithName =
          result.computeIfAbsent(method.getSimpleName(), ignored -> new ArrayList<>());
      methodsWithName.add(method);
    }

    return result;
  }
}
