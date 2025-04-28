/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.custom

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KFunStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.GenerationContext
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.StubsGenerator

private val PARCEL: KStub = KStub("android.os", "Parcel")
private val PARCELABLE_CREATOR: KStub =
    KStub("Creator").apply {
      funStubs += listOf(KFunStub("newArray", 1), KFunStub("createFromParcel", 1))
    }

class ParcelizeStubsGenerator : StubsGenerator {
  override fun generateStubs(context: GenerationContext) {
    val parcelableStub = context.stubsContainer.find("android.os", "Parcelable")

    // We don't have Parcelize-compiler classes in this branch
    // Will stub additional compile-time generated classes
    if (parcelableStub != null) {
      // PARCELABLE_CREATOR might be used somewhere as an interface type
      parcelableStub.innerStubs
          .find { it == PARCELABLE_CREATOR }
          ?.let { it.funStubs += PARCELABLE_CREATOR.funStubs }
          ?: parcelableStub.innerStubs.add(PARCELABLE_CREATOR)

      if (context.stubsContainer.find(PARCEL.pkg!!, PARCEL.name) == null) {
        context.stubsContainer.add(PARCEL)
      }
    }
  }
}
