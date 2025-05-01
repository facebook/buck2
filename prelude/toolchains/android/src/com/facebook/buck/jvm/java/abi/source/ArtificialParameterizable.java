/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import java.util.List;
import javax.lang.model.element.Parameterizable;

/**
 * Interface for {@link Parameterizable}s that are implemented by something other than javac itself.
 */
interface ArtificialParameterizable extends Parameterizable, ArtificialElement {
  @Override
  List<? extends ArtificialTypeParameterElement> getTypeParameters();
}
