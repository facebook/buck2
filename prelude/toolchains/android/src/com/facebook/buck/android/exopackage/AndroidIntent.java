/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.exopackage;

import com.facebook.infer.annotation.Nullsafe;

/** Data class for parameters to a `adb shell am start` command. */
@Nullsafe(Nullsafe.Mode.LOCAL)
public class AndroidIntent {

  public static final String ACTION_MAIN = "android.intent.action.MAIN";
  public static final String ACTION_VIEW = "android.intent.action.VIEW";
  public static final String CATEGORY_LAUNCHER = "android.intent.category.LAUNCHER";

  public final String packageName;
  public final String componentName;
  public final String action;
  public final String category;
  public final String dataUri;
  public final String flags;
  public final boolean waitForDebugger;
  public final boolean skipSetDebugApp;

  public AndroidIntent(
      String packageName,
      String componentName,
      String action,
      String category,
      String dataUri,
      String flags,
      boolean waitForDebugger,
      boolean skipSetDebugApp) {
    this.packageName = packageName;
    this.componentName = componentName;
    this.action = action;
    this.category = category;
    this.dataUri = dataUri;
    this.flags = flags;
    this.waitForDebugger = waitForDebugger;
    this.skipSetDebugApp = skipSetDebugApp;
  }

  /**
   * @return the `am start` command for this intent as a String
   */
  public static String getAmStartCommand(AndroidIntent intent) {
    final StringBuilder builder = new StringBuilder("am start ");
    if (intent.flags != null) {
      builder.append("-f ").append(intent.flags).append(" ");
    }
    if (intent.action != null) {
      builder.append("-a ").append(intent.action).append(" ");
    }
    if (intent.category != null) {
      builder.append("-c ").append(intent.category).append(" ");
    }
    if (intent.dataUri != null) {
      builder.append("-d ").append(intent.dataUri).append(" ");
    }
    if (intent.componentName != null) {
      builder.append("-n ").append(intent.componentName).append(" ");
    }
    if (intent.waitForDebugger) {
      builder.append("-D ");
    }
    return builder.toString();
  }
}
