/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/**
 * Enables generation of ABI jars using only the Java source code of a single target, without
 * requiring access to the source or ABI of dependencies.
 *
 * <p>If the ABI of a dependency target is available, it will be used. However, when one or more
 * dependency ABIs are missing, certain language constructs become ambiguous, and we have to make
 * assumptions. Most of those assumptions can be worked around with small changes to coding style.
 *
 * <p>See {@link com.facebook.buck.jvm.java.abi.source.InterfaceValidator} for more information on
 * the restrictions.
 */
package com.facebook.buck.jvm.java.abi.source;
