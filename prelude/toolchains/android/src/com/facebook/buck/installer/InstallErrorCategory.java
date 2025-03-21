/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer;

import com.facebook.buck.install.model.ErrorCategory;

public enum InstallErrorCategory {
  UNKNOWN,
  INFRA,
  USER,
  ENVIRONMENT;

  public ErrorCategory toProtoModel() {
    switch (this) {
      case INFRA:
        return ErrorCategory.TIER_0;
      case USER:
        return ErrorCategory.INPUT;
      case ENVIRONMENT:
        return ErrorCategory.ENVIRONMENT;
      default:
        return ErrorCategory.ERROR_CATEGORY_UNSPECIFIED;
    }
  }
}
