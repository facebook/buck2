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

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.facebook.demoapp.model.PostModel
import com.facebook.demoapp.ui.theme.PurpleGrey80

@Composable
fun Post(model: PostModel) {
  Card(
      colors =
          CardDefaults.cardColors(
              containerColor = PurpleGrey80,
          ),
      modifier = Modifier.fillMaxWidth().padding(vertical = 24.dp, horizontal = 24.dp),
  ) {
    Text(
        text = model.authorName,
        modifier = Modifier.padding(vertical = 4.dp, horizontal = 8.dp),
        style = TextStyle(fontWeight = FontWeight.Bold, fontSize = 24.sp))
    Image(
        painter = painterResource(id = model.imageResId),
        contentDescription = null,
        modifier = Modifier.fillMaxWidth(),
        contentScale = ContentScale.FillWidth,
    )
    Text(
        text = model.bodyText,
        style = TextStyle(fontFamily = FontFamily.Serif, fontSize = 18.sp),
        modifier = Modifier.padding(horizontal = 8.dp, vertical = 8.dp),
    )
    ActionsRow(model)
  }
}

@Composable
fun ActionsRow(model: PostModel) {
  var likedByMe by remember { mutableStateOf(false) }
  var renderedLikeCount = model.likeCount
  if (likedByMe) {
    renderedLikeCount++
  }
  Row(Modifier.fillMaxWidth()) {
    TextButton(
        onClick = { likedByMe = !likedByMe },
        modifier = Modifier.weight(1f),
    ) {
      val fontWeight = if (likedByMe) FontWeight.Bold else FontWeight.Normal
      Text(
          text = "\uD83D\uDC4D Like (${renderedLikeCount})",
          style = TextStyle(fontWeight = fontWeight, fontSize = 18.sp))
    }
    TextButton(
        onClick = {},
        modifier = Modifier.weight(1f),
    ) {
      Text(text = "Comment", style = TextStyle(fontSize = 18.sp))
    }
  }
}
