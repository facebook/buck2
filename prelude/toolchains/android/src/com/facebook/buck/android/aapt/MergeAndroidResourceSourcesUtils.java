/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.aapt;

import com.android.ide.common.rendering.api.ResourceNamespace;
import com.android.ide.common.resources.MergedResourceWriter;
import com.android.ide.common.resources.MergingException;
import com.android.ide.common.resources.NoOpResourcePreprocessor;
import com.android.ide.common.resources.ResourceMerger;
import com.android.ide.common.resources.ResourceSet;
import com.android.utils.ILogger;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import java.nio.file.Path;

/** Merges multiple directories containing Android resource sources into one directory. */
class MergeAndroidResourceSourcesUtils {

  public static void mergeResPaths(
      ImmutableList<Path> resPaths, Path outFolderPath, Path tmpFolderPath, ILogger logger)
      throws MergingException {
    ResourceMerger merger = new ResourceMerger(1);
    for (Path resPath : resPaths) {
      ResourceSet set =
          new ResourceSet(
              resPath.toString(),
              ResourceNamespace.RES_AUTO,
              /* libraryName */ null,
              /* validateEnabled */ true,
              /* aaptEnv */ null);
      set.setDontNormalizeQualifiers(true);
      set.addSource(resPath.toFile());
      set.loadFromFiles(logger);
      merger.addDataSet(set);
    }
    MergedResourceWriter writer =
        MergedResourceWriter.createWriterWithoutPngCruncher(
            outFolderPath.toFile(),
            null /*publicFile*/,
            null /*blameLogFolder*/,
            NoOpResourcePreprocessor.INSTANCE,
            tmpFolderPath.toFile(),
            ImmutableMap.of() /* moduleSourceSet */,
            "unused_package_name");
    merger.mergeData(writer, /* cleanUp */ false);
  }
}
