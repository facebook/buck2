/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers

// TODO(T175282177): Refactor violation to expression report without calculate the line number
/**
 * Source-only abi code violation
 *
 * @param textOffset the text offset of the line with the violation
 * @param extra any extra data that should be appended to the violation line error message
 */
@Suppress("detekt.EmptyClassBlock") class Violation(val textOffset: Int, val extra: String = "") {}
