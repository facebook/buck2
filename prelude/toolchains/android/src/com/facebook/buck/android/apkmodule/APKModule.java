/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.apkmodule;

import com.facebook.buck.core.util.immutables.BuckStyleValue;
import org.immutables.value.Value;

@BuckStyleValue
public abstract class APKModule implements Comparable<APKModule> {

  public static final String ROOT_APKMODULE_NAME = "dex";

  public static APKModule of(String name) {
    return ImmutableAPKModule.ofImpl(name);
  }

  public abstract String getName();

  @Value.Derived
  public boolean isRootModule() {
    return isRootModule(getName());
  }

  public static boolean isRootModule(String moduleName) {
    return moduleName.equals(ROOT_APKMODULE_NAME);
  }

  @Value.Derived
  public String getCanaryClassName() {
    if (isRootModule()) {
      return "secondary";
    } else {
      return String.format("store%04x", getName().hashCode() & 0xFFFF);
    }
  }

  @Override
  public int compareTo(APKModule o) {
    if (this == o) {
      return 0;
    }

    return getName().compareTo(o.getName());
  }
}
