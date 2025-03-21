/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source.api;

import com.facebook.buck.util.liteinfersupport.Nullable;
import javax.lang.model.element.Element;
import javax.lang.model.util.Elements;
import javax.tools.JavaFileManager;

/** Used to create the SourceOnlyAbiRuleInfo for a target given a JavaFileManager. */
public interface SourceOnlyAbiRuleInfoFactory {
  SourceOnlyAbiRuleInfo create(JavaFileManager fileManager);

  /** Provides information related to source-only abi support. */
  interface SourceOnlyAbiRuleInfo {

    @Nullable
    String getOwningTarget(Elements elements, Element element);

    boolean elementIsAvailableForSourceOnlyAbi(Elements elements, Element element);

    String getRuleName();

    boolean ruleIsRequiredForSourceOnlyAbi();
  }
}
