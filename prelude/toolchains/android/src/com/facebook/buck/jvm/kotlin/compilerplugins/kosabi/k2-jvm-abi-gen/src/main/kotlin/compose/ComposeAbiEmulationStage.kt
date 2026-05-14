/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:SuppressWarnings("PackageLocationMismatch")

package com.facebook

import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension

/**
 * Pipeline stage for Compose ABI emulation.
 *
 * Creates the [ComposeAbiEmulationExtension] which is registered as an [IrGenerationExtension]
 * during FIR-to-IR conversion. The extension runs before the IR sanitizer so that any
 * Compose-emulated declarations are present when non-ABI content is stripped.
 */
internal class ComposeAbiEmulationStage : AbiGenStage {
  override val name = "ComposeAbiEmulation"

  /** Create the IR generation extension to be registered during FIR-to-IR conversion. */
  fun createExtension(): IrGenerationExtension {
    return ComposeAbiEmulationExtension()
  }
}
