/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.demoapp.ui

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.material3.HorizontalDivider
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import com.facebook.demoapp.model.PostModel

@Composable
fun SocialFeed(postModels: List<PostModel>, modifier: Modifier = Modifier) {
  LazyColumn(modifier.fillMaxSize()) {
    itemsIndexed(postModels) { index, model ->
      Post(model)
      if (index != postModels.size - 1) {
        HorizontalDivider()
      }
    }
  }
}
