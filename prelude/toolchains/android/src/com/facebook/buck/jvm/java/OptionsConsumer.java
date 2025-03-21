/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import java.util.Collection;

public interface OptionsConsumer {
  void addOptionValue(String option, String value);

  void addFlag(String flagName);

  void addExtras(Collection<String> extras);
}
