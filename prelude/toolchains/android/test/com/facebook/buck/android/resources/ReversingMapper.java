/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.resources;

import java.nio.IntBuffer;

class ReversingMapper implements ReferenceMapper {
  private final ResourceTable resourceTable;

  public ReversingMapper(ResourceTable resourceTable) {
    this.resourceTable = resourceTable;
  }

  public static ReferenceMapper construct(ResourceTable resourceTable) {
    return new ReversingMapper(resourceTable);
  }

  @Override
  public int map(int id) {
    int resPackage = id >> 24;
    if (resPackage != ResTablePackage.APP_PACKAGE_ID) {
      return id;
    }
    int resType = (id >> 16) & 0xFF;
    int resId = id & 0xFFFF;
    int entryCount = resourceTable.getPackage().getTypeSpec(resType).getEntryCount();
    return (id & 0xFFFF0000) | (entryCount - resId - 1);
  }

  @Override
  public void rewrite(int type, IntBuffer buf) {
    for (int i = 0; i < buf.limit() / 2; i++) {
      int o = buf.limit() - i - 1;
      int t = buf.get(i);
      buf.put(i, buf.get(o));
      buf.put(o, t);
    }
  }
}
