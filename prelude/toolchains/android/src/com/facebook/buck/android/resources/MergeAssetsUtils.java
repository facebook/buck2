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

import com.facebook.buck.android.apkmodule.APKModule;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.hash.HashCode;
import com.google.common.hash.Hashing;
import com.google.common.io.ByteSource;
import com.google.common.io.Files;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.zip.Deflater;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/** Utils for merging assets into an apk. */
public class MergeAssetsUtils {
  public enum BinaryType {
    APK,
    AAB,
  }

  // See
  // https://android.googlesource.com/platform/frameworks/base.git/+/nougat-release/tools/aapt/Package.cpp
  private static final ImmutableSet<String> DEFAULT_NO_COMPRESS_EXTENSIONS =
      ImmutableSet.of(
          "jpg", "jpeg", "png", "gif", "wav", "mp2", "mp3", "ogg", "aac", "mpg", "mpeg", "mid",
          "midi", "smf", "jet", "rtttl", "imy", "xmf", "mp4", "m4a", "m4v", "3gp", "3gpp", "3g2",
          "3gpp2", "amr", "awb", "wma", "wmv", "webm", "mkv", "tflite");

  /**
   * Construct an APK containing assets. If a "baseApk" was provided, also include everything from
   * that APK.
   */
  public static void mergeAssets(
      Path outputApk,
      Optional<Path> moduleAssetsOutputDir,
      Optional<Path> baseApk,
      AbsPath root,
      ImmutableMap<String, ImmutableSet<RelPath>> moduleToAssetsDirectories,
      ImmutableSet<String> extraNoCompressExtensions,
      BinaryType binaryType)
      throws IOException {
    ImmutableSet<String> allNoCompressExtensions =
        new ImmutableSet.Builder<String>()
            .addAll(DEFAULT_NO_COMPRESS_EXTENSIONS)
            .addAll(extraNoCompressExtensions)
            .build();
    if (moduleAssetsOutputDir.isPresent()) {
      Preconditions.checkState(binaryType == BinaryType.AAB);
      java.nio.file.Files.createDirectories(moduleAssetsOutputDir.get());
    }

    try (ResourcesZipBuilder outputApkResources =
        new ResourcesZipBuilder(outputApk, /* addManifestIfMissing */ true)) {
      if (baseApk.isPresent()) {
        try (ZipFile base = new ZipFile(baseApk.get().toFile())) {
          for (ZipEntry inputEntry : Collections.list(base.entries())) {
            String extension = Files.getFileExtension(inputEntry.getName());
            // Only compress if aapt compressed it and the extension looks compressible.
            // This is a workaround for aapt2 compressing everything.
            boolean shouldCompress =
                inputEntry.getMethod() != ZipEntry.STORED
                    && !allNoCompressExtensions.contains(extension);
            try (InputStream stream = base.getInputStream(inputEntry)) {
              outputApkResources.addEntry(
                  stream,
                  inputEntry.getSize(),
                  inputEntry.getCrc(),
                  inputEntry.getName(),
                  shouldCompress ? Deflater.BEST_COMPRESSION : 0,
                  inputEntry.isDirectory());
            }
          }
        }
      }

      for (Map.Entry<String, ImmutableSet<RelPath>> entry : moduleToAssetsDirectories.entrySet()) {
        String moduleName = entry.getKey();
        ImmutableMap<Path, Path> assets = getAllAssets(root, entry.getValue());
        boolean isRootModule = APKModule.isRootModule(moduleName);
        if (isRootModule || binaryType == BinaryType.APK) {
          Path assetsZipRoot =
              isRootModule
                  ? Paths.get("assets")
                  : Paths.get("assets").resolve(moduleName).resolve("assets");
          addAllAssets(outputApkResources, assets, assetsZipRoot, allNoCompressExtensions);
        } else {
          Path moduleDir = moduleAssetsOutputDir.orElseThrow().resolve(moduleName);
          java.nio.file.Files.createDirectory(moduleDir);
          try (ResourcesZipBuilder moduleOutputApk =
              new ResourcesZipBuilder(
                  moduleDir.resolve("assets.ap_"), /* addManifestIfMissing */ false)) {
            Path assetsZipRoot = Paths.get("assets").resolve(moduleName);
            addAllAssets(moduleOutputApk, assets, assetsZipRoot, allNoCompressExtensions);
          }
        }
      }
    }
  }

  private static ImmutableMap<Path, Path> getAllAssets(
      AbsPath root, ImmutableSet<RelPath> assetsDirectories) throws IOException {
    ImmutableMap.Builder<Path, Path> assets = ImmutableMap.builder();

    for (RelPath assetDirectory : assetsDirectories) {
      AbsPath absolutePath = ProjectFilesystemUtils.getAbsPathForRelativePath(root, assetDirectory);

      ProjectFilesystemUtils.walkFileTree(
          root,
          assetDirectory.getPath(),
          ImmutableSet.of(FileVisitOption.FOLLOW_LINKS),
          new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                throws IOException {
              Preconditions.checkState(
                  !Files.getFileExtension(file.toString()).equals("gz"),
                  "BUCK doesn't support adding .gz files to assets (%s).",
                  file);
              Path normalized = file.normalize();
              assets.put(absolutePath.getPath().relativize(normalized), normalized);
              return super.visitFile(file, attrs);
            }
          },
          ProjectFilesystemUtils.getEmptyIgnoreFilter());
    }

    return assets.build();
  }

  private static void addAllAssets(
      ResourcesZipBuilder output,
      ImmutableMap<Path, Path> assets,
      Path assetsZipRoot,
      ImmutableSet<String> allNoCompressExtensions)
      throws IOException {
    for (Map.Entry<Path, Path> assetPaths : assets.entrySet()) {
      Path packagingPathForAsset = assetPaths.getKey();
      Path fullPathToAsset = assetPaths.getValue();
      ByteSource assetSource = Files.asByteSource(fullPathToAsset.toFile());
      HashCode assetCrc32 = assetSource.hash(Hashing.crc32());
      String extension = Files.getFileExtension(fullPathToAsset.toString());
      int compression = allNoCompressExtensions.contains(extension) ? 0 : Deflater.BEST_COMPRESSION;
      try (InputStream assetStream = assetSource.openStream()) {
        output.addEntry(
            assetStream,
            assetSource.size(),
            // CRC32s are only 32 bits, but setCrc() takes a
            // long.  Avoid sign-extension here during the
            // conversion to long by masking off the high 32 bits.
            assetCrc32.asInt() & 0xFFFFFFFFL,
            assetsZipRoot.resolve(packagingPathForAsset).toString(),
            compression,
            false);
      }
    }
  }
}
