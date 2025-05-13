/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
