/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.facebook.buck.jvm.java.abi.source.api.SourceOnlyAbiRuleInfoFactory.SourceOnlyAbiRuleInfo;
import com.facebook.buck.jvm.java.lang.model.MoreElements;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;

public class TestSourceOnlyAbiRuleInfo implements SourceOnlyAbiRuleInfo {
  private final String ruleName;
  private final boolean requiredForSourceOnlyAbi = false;
  private final Map<String, String> owningRules = new HashMap<>();
  private final Set<String> rulesAvailableForSourceOnlyAbi = new HashSet<>();

  public TestSourceOnlyAbiRuleInfo(String ruleName) {
    this.ruleName = ruleName;
  }

  public void addElementOwner(String qualifiedName, String owningRule) {
    owningRules.put(qualifiedName, owningRule);
  }

  public void addAvailableRule(String ruleName) {
    rulesAvailableForSourceOnlyAbi.add(ruleName);
  }

  @Override
  public boolean elementIsAvailableForSourceOnlyAbi(Elements elements, Element element) {
    if (MoreElements.getPackageElement(element).getQualifiedName().contentEquals("java.lang")) {
      return true;
    }
    return rulesAvailableForSourceOnlyAbi.contains(getOwningTarget(elements, element));
  }

  @Override
  public String getOwningTarget(Elements elements, Element element) {
    TypeElement typeElement = MoreElements.getTopLevelTypeElement(element);
    return owningRules.get(typeElement.getQualifiedName().toString());
  }

  @Override
  public String getRuleName() {
    return ruleName;
  }

  @Override
  public boolean ruleIsRequiredForSourceOnlyAbi() {
    return requiredForSourceOnlyAbi;
  }
}
