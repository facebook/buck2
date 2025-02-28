/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source.api;

/**
 * Thrown when a source ABI cannot be generated because the source code will not compile. Correct
 * handling is to simply stop attempting to generate the ABI and allow the compiler to fail.
 */
public class SourceCodeWillNotCompileException extends RuntimeException {}
