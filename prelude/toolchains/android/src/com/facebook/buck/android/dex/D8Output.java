/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.dex;

import java.util.Collection;

/** Info exposed from {@code d8}. */
public class D8Output {
  /** Referenced resources returned by D8 */
  private Collection<String> referencedResources;

  /** Synthetic classes generated by D8 */
  private Collection<String> referencedSynthetic;

  public D8Output(Collection<String> referencedResources, Collection<String> referencedSynthetic) {
    this.referencedResources = referencedResources;
    this.referencedSynthetic = referencedSynthetic;
  }

  public Collection<String> getResources() {
    return referencedResources;
  }

  public Collection<String> getSynthetic() {
    return referencedSynthetic;
  }
}
