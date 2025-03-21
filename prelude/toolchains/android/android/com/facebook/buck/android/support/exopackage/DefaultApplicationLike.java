/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.support.exopackage;

import android.app.Application;
import android.content.res.Configuration;

/** Empty implementation of {@link ApplicationLike}. */
public class DefaultApplicationLike implements ApplicationLike {
  public DefaultApplicationLike() {}

  @SuppressWarnings("unused")
  public DefaultApplicationLike(Application application) {}

  @Override
  public void onCreate() {}

  @Override
  public void onLowMemory() {}

  @Override
  public void onTrimMemory(int level) {}

  @Override
  public void onTerminate() {}

  @Override
  public void onConfigurationChanged(Configuration newConfig) {}

  @Override
  public Object getSystemService(String name) {
    return null;
  }
}
