/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.demoapp.model

import androidx.annotation.DrawableRes

data class PostModel(
    val authorName: String,
    @DrawableRes val imageResId: Int,
    val bodyText: String,
    val likeCount: Int,
) {
  init {
    require(authorName.isNotEmpty())
    require(imageResId != 0)
  }
}
