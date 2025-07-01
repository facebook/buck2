/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.support.exopackage;

import android.app.Application;
import android.content.res.Configuration;

/**
 * This interface is used to delegate calls from main Application object.
 *
 * <p>Implementations of this interface must have a one-argument constructor that takes an argument
 * of type {@link Application}.
 */
public interface ApplicationLike {

  /** Same as {@link Application#onCreate()}. */
  void onCreate();

  /** Same as {@link Application#onLowMemory()}. */
  void onLowMemory();

  /**
   * Same as {@link Application#onTrimMemory(int level)}.
   *
   * @param level
   */
  void onTrimMemory(int level);

  /** Same as {@link Application#onTerminate()}. */
  void onTerminate();

  /** Same as {@link Application#onConfigurationChanged(Configuration newconfig)}. */
  void onConfigurationChanged(Configuration newConfig);

  /** Same as {@link Application#getSystemService(String name)}. */
  Object getSystemService(String name);
}
