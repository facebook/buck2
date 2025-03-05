/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java

import com.facebook.buck.jvm.java.abi.source.api.SourceOnlyAbiRuleInfoFactory
import com.facebook.buck.jvm.java.abi.source.api.SourceOnlyAbiRuleInfoFactory.SourceOnlyAbiRuleInfo
import javax.tools.JavaFileManager

/** Default factory for SourceOnlyAbiRuleInfos. */
data class DefaultSourceOnlyAbiRuleInfoFactory(
    val fullyQualifiedBuildTargetName: String,
    val ruleIsRequiredForSourceOnlyAbi: Boolean
) : SourceOnlyAbiRuleInfoFactory {
  override fun create(fileManager: JavaFileManager): SourceOnlyAbiRuleInfo {
    return DefaultSourceOnlyAbiRuleInfo(
        fileManager, fullyQualifiedBuildTargetName, ruleIsRequiredForSourceOnlyAbi)
  }
}
