/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.facebook.buck.util.liteinfersupport.Nullable;
import java.util.List;
import javax.lang.model.element.Element;

/** Interface for {@link Element}s that are implemented by something other than javac itself. */
interface ArtificialElement extends Element {
  @Override
  @Nullable
  ArtificialElement getEnclosingElement();

  @Override
  List<? extends Element> getEnclosedElements();

  void addEnclosedElement(Element element);
}
