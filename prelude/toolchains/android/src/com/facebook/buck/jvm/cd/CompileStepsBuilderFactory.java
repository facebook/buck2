/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd;

/** Factory that creates {@link CompileStepsBuilder} instances */
public interface CompileStepsBuilderFactory {

  /** Creates an appropriate {@link LibraryStepsBuilder} instance */
  LibraryStepsBuilder getLibraryBuilder();

  /** Creates an appropriate {@link AbiStepsBuilder} instance */
  AbiStepsBuilder getAbiBuilder();
}
