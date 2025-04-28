/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.Supplier;

/** Installs native code for exo. */
public class NativeExoHelper implements ExoHelper {
  @VisibleForTesting public static final Path NATIVE_LIBS_DIR = Paths.get("native-libs");
  private final Supplier<List<String>> abiSupplier;
  private final AbsPath rootPath;
  private final IsolatedExopackageInfo.IsolatedNativeLibsInfo nativeLibsInfo;

  NativeExoHelper(
      Supplier<List<String>> abiSupplier,
      AbsPath rootPath,
      IsolatedExopackageInfo.IsolatedNativeLibsInfo nativeLibsInfo) {
    this.abiSupplier = abiSupplier;
    this.rootPath = rootPath;
    this.nativeLibsInfo = nativeLibsInfo;
  }

  @Override
  public String getType() {
    return "native_library";
  }

  /**
   * @return a mapping from destinationPathOnDevice -> localPath
   */
  @Override
  public ImmutableMap<Path, Path> getFilesToInstall() throws IOException {
    ImmutableMap.Builder<Path, Path> filesToInstallBuilder = ImmutableMap.builder();
    ImmutableMap<String, ImmutableMultimap<String, Path>> filesByHashForAbis =
        getFilesByHashForAbis();
    for (String abi : filesByHashForAbis.keySet()) {
      ImmutableMultimap<String, Path> filesByHash =
          Objects.requireNonNull(filesByHashForAbis.get(abi));
      Path abiDir = NATIVE_LIBS_DIR.resolve(abi);
      for (Entry<Path, Collection<Path>> entry :
          ExopackageUtil.applyFilenameFormat(filesByHash, abiDir, "native-%s.so")
              .asMap()
              .entrySet()) {
        // The files in the getValue collection should all be identical
        // (because the key in their hash), so just pick the first one.
        filesToInstallBuilder.put(entry.getKey(), entry.getValue().iterator().next());
      }
    }
    return filesToInstallBuilder.build();
  }

  /**
   * @return a mapping from destinationPathOnDevice -> contents of file for all native-libs metadata
   *     files (one per abi)
   */
  @Override
  public ImmutableMap<Path, String> getMetadataToInstall() throws IOException {
    ImmutableMap<String, ImmutableMultimap<String, Path>> filesByHashForAbis =
        getFilesByHashForAbis();
    ImmutableMap.Builder<Path, String> metadataBuilder = ImmutableMap.builder();
    for (String abi : filesByHashForAbis.keySet()) {
      ImmutableMultimap<String, Path> filesByHash =
          Objects.requireNonNull(filesByHashForAbis.get(abi));
      Path abiDir = NATIVE_LIBS_DIR.resolve(abi);
      metadataBuilder.put(
          abiDir.resolve("metadata.txt"), getNativeLibraryMetadataContents(filesByHash));
    }
    return metadataBuilder.build();
  }

  private ImmutableMultimap<String, Path> getAllLibraries() throws IOException {
    return ExopackageInstaller.parseExopackageInfoMetadata(
        nativeLibsInfo.getMetadata(), nativeLibsInfo.getDirectory(), rootPath);
  }

  private ImmutableMap<String, ImmutableMultimap<String, Path>> getFilesByHashForAbis()
      throws IOException {
    List<String> deviceAbis = abiSupplier.get();
    ImmutableMap.Builder<String, ImmutableMultimap<String, Path>> filesByHashForAbisBuilder =
        ImmutableMap.builder();
    ImmutableMultimap<String, Path> allLibraries = getAllLibraries();
    ImmutableSet.Builder<String> providedLibraries = ImmutableSet.builder();
    for (String abi : deviceAbis) {
      ImmutableMultimap<String, Path> filesByHash =
          getRequiredLibrariesForAbi(allLibraries, abi, providedLibraries.build());
      if (filesByHash.isEmpty()) {
        continue;
      }
      providedLibraries.addAll(filesByHash.keySet());
      filesByHashForAbisBuilder.put(abi, filesByHash);
    }
    return filesByHashForAbisBuilder.build();
  }

  private ImmutableMultimap<String, Path> getRequiredLibrariesForAbi(
      ImmutableMultimap<String, Path> allLibraries,
      String abi,
      ImmutableSet<String> ignoreLibraries) {
    return filterLibrariesForAbi(nativeLibsInfo.getDirectory(), allLibraries, abi, ignoreLibraries);
  }

  @VisibleForTesting
  public static ImmutableMultimap<String, Path> filterLibrariesForAbi(
      AbsPath nativeLibsDir,
      ImmutableMultimap<String, Path> allLibraries,
      String abi,
      ImmutableSet<String> ignoreLibraries) {
    ImmutableMultimap.Builder<String, Path> filteredLibraries = ImmutableMultimap.builder();
    for (Map.Entry<String, Path> entry : allLibraries.entries()) {
      RelPath relativePath = nativeLibsDir.relativize(entry.getValue());
      Preconditions.checkState(
          relativePath.getNameCount() == 2 || relativePath.getNameCount() == 3,
          "relativePath should be of the form x86/foo.so (for buck2) or either libs/x86/foo.so or"
              + " assetLibs/x86/foo.so (for buck1), but was: "
              + relativePath);
      if (relativePath.getNameCount() == 3) {
        Preconditions.checkState(
            relativePath.getName(0).toString().equals("libs")
                || relativePath.getName(0).toString().equals("assetLibs"));
      }
      String libAbi = relativePath.getParent().getFileName().toString();
      String libName = relativePath.getFileName().toString();
      if (libAbi.equals(abi) && !ignoreLibraries.contains(libName)) {
        filteredLibraries.put(entry);
      }
    }
    return filteredLibraries.build();
  }

  private String getNativeLibraryMetadataContents(ImmutableMultimap<String, Path> libraries) {
    return Joiner.on('\n')
        .join(
            FluentIterable.from(libraries.entries())
                .transform(
                    input -> {
                      String hash = input.getKey();
                      String filename = input.getValue().getFileName().toString();
                      int index = filename.lastIndexOf('.');
                      String libname = index == -1 ? filename : filename.substring(0, index);
                      return String.format("%s native-%s.so", libname, hash);
                    }));
  }
}
