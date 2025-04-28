/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.k2

import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.FeatureChecker
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.extensions.FirExtensionRegistrar

class FirApplicabilityExtensionRegistrar(private val checkers: List<FeatureChecker>) :
    FirExtensionRegistrar() {
  private fun createExtension(session: FirSession) = ApplicabilityPluginExtension(session, checkers)

  override fun ExtensionRegistrarContext.configurePlugin() {
    +::createExtension
  }
}
