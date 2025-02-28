/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.aapt;

/**
 * An {@link RDotTxtEntry} with fake {@link #idValue}, useful for comparing two resource entries for
 * equality, since {@link RDotTxtEntry#compareTo(RDotTxtEntry)} ignores the id value.
 */
public class FakeRDotTxtEntry extends RDotTxtEntry {

  private static final String FAKE_ID = "0x00000000";

  public FakeRDotTxtEntry(IdType idType, RType type, String name) {
    super(idType, type, name, FAKE_ID);
  }
}
