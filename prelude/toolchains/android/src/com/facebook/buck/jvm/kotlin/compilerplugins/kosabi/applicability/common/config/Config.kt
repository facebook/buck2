/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.config

object Config {
  val blockImportList: Set<String> =
      setOf(
          // Compose compiler plugin
          "androidx.compose.runtime.Composable",
          // React Module AP
          "com.facebook.react.module.annotations.ReactModule",
          "com.facebook.react.module.annotations.ReactModuleList",
          // Known KSP
          // Ultralight static annotations below expect paired annotation to work
          // TODO(T164251483): Support special rules for Ultralight static annotations
          "com.facebook.inject.statics.AssignBoundListStatic",
          // Modelgen
          "com.facebook.annotationprocessors.modelgen.iface.ModelField",
          "com.facebook.annotationprocessors.modelgen.iface.ModelDefinition",
          // Known unsupported Metagen libraries
          "com.facebook.crudolib.urimap.componenthelper.impl.ComponentHelperFactoryEntry",
          // Deprecated API, please direct user to use Ultralight Static
          "com.facebook.common.plugins.Plugins",
          // Known KAPT annotation processors
          // Storage configs
          "com.facebook.storage.annotation.StorageConfigs",
      )

  /** Prefix level block imports */
  val blockImportPrefixList: Set<String> =
      setOf(
          "androidx.room", // Room AP around 30+ annotation classes : https://fburl.com/cm1b3llc
      )

  val tolerateAdditionalJavaSourcePath: List<String> =
      listOf(
          // BUCK generated files, likely KAPT
          "buck-out")
}
