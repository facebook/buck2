/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin;

/** Configuration for Kosabi plugin. */
public abstract class KosabiConfig {
  public static final String PROPERTY_KOSABI_APPLICABILITY_PLUGIN = "kosabi_applicability_plugin";
  public static final String PROPERTY_KOSABI_STUBS_GEN_PLUGIN = "kosabi_stubs_gen_plugin";
  public static final String PROPERTY_KOSABI_SOURCE_MODIFIER_PLUGIN =
      "kosabi_source_modifier_plugin";
  public static final String PROPERTY_KOSABI_JVM_ABI_GEN_PLUGIN = "kosabi_jvm_abi_gen_plugin";

  public static final String PROPERTY_KOSABI_STANDALONE = "kosabi_standalone";
}
