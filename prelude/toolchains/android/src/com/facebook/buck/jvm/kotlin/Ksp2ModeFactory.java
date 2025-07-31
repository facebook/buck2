/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.ksp.incremental.Ksp2Mode;
import java.util.Collections;
import java.util.Optional;

public class Ksp2ModeFactory {
  private static final Logger LOG = Logger.get(Ksp2ModeFactory.class);

  public static Ksp2Mode create(
      boolean isSourceOnly,
      final RelPath kspCachesOutput,
      final KotlinExtraParams extraParams,
      final Optional<ActionMetadata> actionMetadata) {
    if (!extraParams.getShouldKsp2RunIncrementally()) {
      LOG.info("Non-incremental mode applied: incremental property disabled");
      return new Ksp2Mode.NonIncremental(kspCachesOutput);
    } else if (isSourceOnly) {
      LOG.info("Non-incremental mode applied: source-only build requested");
      return new Ksp2Mode.NonIncremental(kspCachesOutput);
    } else {
      AbsPath cachesDir =
          extraParams
              .getKsp2CachesDir()
              .orElseThrow(
                  () ->
                      new IllegalStateException(
                          "incremental_state_dir/ksp2_caches_dir is not created"));

      // TODO(ijurcikova) use metadata for calculation of changes
      ActionMetadata metadata =
          actionMetadata.orElseThrow(
              () -> new IllegalStateException("actionMetadata is not created"));

      LOG.info("Incremental mode applied");

      return new Ksp2Mode.Incremental(
          cachesDir,
          true,
          Collections.emptyList(),
          Collections.emptyList(),
          Collections.emptyList());
    }
  }
}
