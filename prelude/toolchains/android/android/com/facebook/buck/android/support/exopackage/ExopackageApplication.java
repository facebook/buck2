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

import android.annotation.TargetApi;
import android.app.Application;
import android.content.Context;
import android.content.res.Configuration;
import java.lang.reflect.Constructor;

/**
 * A base class for implementing an Application that delegates to an {@link ApplicationLike}
 * instance. This is used in conjunction with secondary dex files so that the logic that would
 * normally live in the Application class is loaded after the secondary dexes are loaded.
 */
public abstract class ExopackageApplication<T extends ApplicationLike> extends Application {

  protected static final int SECONDARY_DEX_MASK = 1;
  protected static final int NATIVE_LIBRARY_MASK = 2;
  protected static final int RESOURCES_MASK = 4;
  protected static final int MODULES_MASK = 8;

  private final String delegateClassName;
  private final int exopackageFlags;
  private T delegate;

  /**
   * @param exopackageFlags Bitmask used to determine which exopackage feature is enabled in the
   *     current build. This should usually be {@code BuildConfig.EXOPACKAGE_FLAGS}.
   */
  protected ExopackageApplication(int exopackageFlags) {
    this(DefaultApplicationLike.class.getName(), exopackageFlags);
  }

  /**
   * @param delegateClassName The fully-qualified name of the {@link ApplicationLike} class that
   *     will act as the delegate for application lifecycle callbacks.
   * @param exopackageFlags Bitmask used to determine which exopackage feature is enabled in the
   *     current build. This should usually be {@code BuildConfig.EXOPACKAGE_FLAGS}.
   */
  protected ExopackageApplication(String delegateClassName, int exopackageFlags) {
    this.delegateClassName = delegateClassName;
    this.exopackageFlags = exopackageFlags;
  }

  protected boolean isExopackageEnabledForSecondaryDex() {
    return (exopackageFlags & SECONDARY_DEX_MASK) != 0;
  }

  protected boolean isExopackageEnabledForNativeLibraries() {
    return (exopackageFlags & NATIVE_LIBRARY_MASK) != 0;
  }

  protected boolean isExopackageEnabledForResources() {
    return (exopackageFlags & RESOURCES_MASK) != 0;
  }

  protected T createDelegate() {
    if (isExopackageEnabledForSecondaryDex()) {
      ExopackageDexLoader.loadExopackageJars(this);
    }

    if (isExopackageEnabledForNativeLibraries()) {
      ExopackageSoLoader.init(this);
    }

    if (isExopackageEnabledForResources()) {
      ResourcesLoader.init(this);
    }

    try {
      // Use reflection to create the delegate so it doesn't need to go into the primary dex.
      Class<T> implClass = (Class<T>) Class.forName(delegateClassName);
      Constructor<T> constructor = implClass.getConstructor(Application.class);
      return constructor.newInstance(this);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private synchronized void ensureDelegate() {
    if (delegate == null) {
      delegate = createDelegate();
    }
  }

  /**
   * Hook for sub-classes to run logic after the {@link Application#attachBaseContext} has been
   * called but before the delegate is created. Implementors should be very careful what they do
   * here since {@link android.app.Application#onCreate} will not have yet been called.
   */
  protected void onBaseContextAttached() {}

  /**
   * @return the delegate, or {@code null} if not set up.
   */
  // @Nullable  - Don't want to force a reference to that annotation in the primary dex.
  public final T getDelegateIfPresent() {
    return delegate;
  }

  @Override
  protected void attachBaseContext(Context base) {
    super.attachBaseContext(base);
    onBaseContextAttached();
    ensureDelegate();
  }

  @Override
  public void onCreate() {
    super.onCreate();
    if (isExopackageEnabledForResources()) {
      ResourcesLoader.onAppCreated(this);
    }
    ensureDelegate();
    delegate.onCreate();
  }

  @Override
  public final void onTerminate() {
    super.onTerminate();
    if (delegate != null) {
      delegate.onTerminate();
    }
  }

  @Override
  public final void onLowMemory() {
    super.onLowMemory();
    if (delegate != null) {
      delegate.onLowMemory();
    }
  }

  @TargetApi(14)
  @Override
  public final void onTrimMemory(int level) {
    super.onTrimMemory(level);
    if (delegate != null) {
      delegate.onTrimMemory(level);
    }
  }

  @Override
  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);
    if (delegate != null) {
      delegate.onConfigurationChanged(newConfig);
    }
  }

  @Override
  public Object getSystemService(String name) {
    if (delegate != null) {
      Object service = delegate.getSystemService(name);
      if (service != null) {
        return service;
      }
    }
    return super.getSystemService(name);
  }
}
