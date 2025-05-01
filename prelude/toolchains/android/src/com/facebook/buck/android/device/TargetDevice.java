/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.device;

import java.util.Optional;

/** Represents information about the device we're targeting. */
public class TargetDevice {

  private final Type type;
  private final Optional<String> identifier;

  public enum Type {
    REAL_DEVICE,
    EMULATOR,
    BY_SERIAL
  }

  public TargetDevice(Type type, Optional<String> identifier) {
    this.type = type;
    this.identifier = identifier;
  }

  public boolean isEmulator() {
    return type == Type.EMULATOR;
  }

  public Optional<String> getIdentifier() {
    return identifier;
  }
}
