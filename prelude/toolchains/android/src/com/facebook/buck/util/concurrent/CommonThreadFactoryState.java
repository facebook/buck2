/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.concurrent;

public interface CommonThreadFactoryState extends ThreadIdToCommandIdMapper {

  CommonThreadFactoryState NOOP =
      new CommonThreadFactoryState() {

        @Override
        public String threadIdToCommandId(long threadId) {
          return "";
        }

        @Override
        public void register(long threadId, String commandId) {}
      };

  void register(long threadId, String commandId);
}
