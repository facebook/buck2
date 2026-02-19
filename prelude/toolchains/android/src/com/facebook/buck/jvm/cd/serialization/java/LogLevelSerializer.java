/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.java;

import com.facebook.buck.cd.model.java.JarParameters;
import com.facebook.buck.jvm.cd.serialization.SerializationUtil;
import com.facebook.infer.annotation.Nullsafe;
import java.util.logging.Level;

/** {@link Level} to protobuf serializer */
@Nullsafe(Nullsafe.Mode.LOCAL)
class LogLevelSerializer {

  private LogLevelSerializer() {}

  /** Deserializes javacd model's {@link JarParameters.LogLevel} into {@link Level}. */
  public static Level deserialize(JarParameters.LogLevel level) {
    switch (level) {
      case ALL:
        return Level.ALL;
      case OFF:
        return Level.OFF;
      case CONFIG:
        return Level.CONFIG;

      case SEVERE:
        return Level.SEVERE;
      case WARNING:
        return Level.WARNING;
      case INFO:
        return Level.INFO;

      case FINE:
        return Level.FINE;
      case FINER:
        return Level.FINER;
      case FINEST:
        return Level.FINEST;

      case UNRECOGNIZED:
      case UNKNOWN:
      default:
        throw SerializationUtil.createNotSupportedException(level);
    }
  }
}
