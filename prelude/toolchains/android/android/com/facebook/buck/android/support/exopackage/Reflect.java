/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.support.exopackage;

import java.lang.reflect.Field;

class Reflect {
  private Reflect() {}

  static Object getField(Object object, Class<?> clazz, String fieldName)
      throws NoSuchFieldException, IllegalAccessException {
    Field field = clazz.getDeclaredField(fieldName);
    field.setAccessible(true);
    return field.get(object);
  }

  static void setField(Object object, Class<?> clazz, String fieldName, Object fieldValue)
      throws NoSuchFieldException, IllegalAccessException {
    Field field = clazz.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(object, fieldValue);
  }
}
