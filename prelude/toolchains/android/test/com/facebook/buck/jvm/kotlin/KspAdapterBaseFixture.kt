/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.ksp.kspadapter

import com.google.devtools.ksp.processing.SymbolProcessor

/**
 * Stands in for the javax adapter's own base class, which lives in fbandroid and is not on this
 * target's classpath. `Ksp2NoOpDetector` recognises adapter processors by this fully-qualified
 * name, so the package and class name here are what the test actually exercises.
 */
abstract class SymbolProcessorBase : SymbolProcessor
