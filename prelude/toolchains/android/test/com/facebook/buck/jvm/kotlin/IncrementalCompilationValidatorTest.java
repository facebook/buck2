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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.RebuildReason;
import java.io.File;
import org.junit.Before;
import org.junit.Test;

public class IncrementalCompilationValidatorTest {

  private IncrementalCompilationValidator incrementalCompilationValidator;

  @Before
  public void setUp() {
    incrementalCompilationValidator = new IncrementalCompilationValidator();
  }

  @Test
  public void
      when_previousMetadataDigestIsNull_then_returnPreviousBuildConfigurationFileNotFoundReason() {
    ActionMetadata actionMetadata = mock(ActionMetadata.class);
    when(actionMetadata.getPreviousIncrementalMetadataDigest()).thenReturn(null);

    RebuildReason result = incrementalCompilationValidator.validate(actionMetadata, null, null);

    assertEquals(RebuildReason.NO_LAST_BUILD_CONFIGURATION, result);
  }

  @Test
  public void when_buildConfigurationChanged_then_returnBuildConfigurationChangedReason() {
    ActionMetadata actionMetadata = mock(ActionMetadata.class);
    when(actionMetadata.getPreviousIncrementalMetadataDigest()).thenReturn("digest1");
    when(actionMetadata.getCurrentIncrementalMetadataDigest()).thenReturn("digest2");

    RebuildReason result = incrementalCompilationValidator.validate(actionMetadata, null, null);

    assertEquals(RebuildReason.BUILD_CONFIGURATION_CHANGED, result);
  }

  @Test
  public void
      when_kotlinClassUsageFileDirNotExists_then_returnPreviousKotlinUsedClassesFileNotFoundReason() {
    ActionMetadata actionMetadata = mock(ActionMetadata.class);
    when(actionMetadata.getPreviousIncrementalMetadataDigest()).thenReturn("digest");
    when(actionMetadata.getCurrentIncrementalMetadataDigest()).thenReturn("digest");
    AbsPath kotlinClassUsageFileDir = mock(AbsPath.class);
    File mockFile = mock(File.class);
    when(kotlinClassUsageFileDir.toFile()).thenReturn(mockFile);
    when(mockFile.exists()).thenReturn(false);

    RebuildReason result =
        incrementalCompilationValidator.validate(actionMetadata, kotlinClassUsageFileDir, null);

    assertEquals(RebuildReason.NO_LAST_KOTLIN_USED_CLASSES_FILE, result);
  }

  @Test
  public void when_jvmAbiGenWorkingDirNotExists_then_returnJvmAbiGenWorkingDirNotFoundReason() {
    ActionMetadata actionMetadata = mock(ActionMetadata.class);
    when(actionMetadata.getPreviousIncrementalMetadataDigest()).thenReturn("digest");
    when(actionMetadata.getCurrentIncrementalMetadataDigest()).thenReturn("digest");
    AbsPath jvmAbiGenWorkingDir = mock(AbsPath.class);
    File mockFile = mock(File.class);
    when(jvmAbiGenWorkingDir.toFile()).thenReturn(mockFile);
    when(mockFile.exists()).thenReturn(false);

    RebuildReason result =
        incrementalCompilationValidator.validate(actionMetadata, null, jvmAbiGenWorkingDir);

    assertEquals(RebuildReason.NO_JVM_ABI_WORKING_DIR, result);
  }

  @Test
  public void when_allConditionsPass_then_returnNull() {
    ActionMetadata actionMetadata = mock(ActionMetadata.class);
    when(actionMetadata.getPreviousIncrementalMetadataDigest()).thenReturn("digest");
    when(actionMetadata.getCurrentIncrementalMetadataDigest()).thenReturn("digest");
    AbsPath kotlinClassUsageFileDir = mock(AbsPath.class);
    AbsPath jvmAbiGenWorkingDir = mock(AbsPath.class);
    File mockFile1 = mock(File.class);
    File mockFile2 = mock(File.class);
    when(kotlinClassUsageFileDir.toFile()).thenReturn(mockFile1);
    when(jvmAbiGenWorkingDir.toFile()).thenReturn(mockFile2);
    when(mockFile1.exists()).thenReturn(true);
    when(mockFile2.exists()).thenReturn(true);

    RebuildReason result =
        incrementalCompilationValidator.validate(
            actionMetadata, kotlinClassUsageFileDir, jvmAbiGenWorkingDir);

    assertNull(result);
  }
}
