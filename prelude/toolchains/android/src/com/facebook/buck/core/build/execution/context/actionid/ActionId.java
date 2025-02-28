/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.build.execution.context.actionid;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import java.util.Objects;

/** Executing action id. Typically represents that fully qualified name of the build target. */
public class ActionId {

  private final String value;

  public ActionId(String value) {
    Preconditions.checkState(!Strings.isNullOrEmpty(value), "action id has to be not empty");
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  @Override
  public int hashCode() {
    return Objects.hash(value);
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }
    return value.equals(((ActionId) obj).value);
  }

  public static ActionId of(String value) {
    return new ActionId(value);
  }
}
