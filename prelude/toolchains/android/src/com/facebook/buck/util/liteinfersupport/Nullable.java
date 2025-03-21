/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.liteinfersupport;

/**
 * Some projects like FatJar is going to be embedded in many targets, it cannot have external
 * dependencies, but we'd like to have {@link javax.annotation.Nullable} and {@link
 * com.google.common.base.Preconditions#checkNotNull} anyway, so we define these here.
 */
public @interface Nullable {}
