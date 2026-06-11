/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd;

import com.facebook.buck.util.json.ObjectMappers;
import com.facebook.infer.annotation.Nullsafe;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.ImmutableSortedSet;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

@Nullsafe(Nullsafe.Mode.LOCAL)
public class DepFileUtils {

  /** Maps used-classes.json files to a dep-file that can be used by buck2. */
  public static void usedClassesToDepFile(
      List<Path> usedClassesMapPaths,
      Path depFileOutput,
      Optional<Path> jarToJarDirMapPath,
      boolean append)
      throws IOException {
    ImmutableMap<Path, Path> jarToJarDirMap;
    if (jarToJarDirMapPath.isPresent()) {
      jarToJarDirMap =
          Files.readAllLines(jarToJarDirMapPath.get()).stream()
              // TODO(ianc) fix this, we shouldn't be adding the same jar to the classpath multiple
              // times
              .distinct()
              .map(line -> line.split(" "))
              .collect(ImmutableMap.toImmutableMap(x -> Paths.get(x[0]), x -> Paths.get(x[1])));
    } else {
      jarToJarDirMap = ImmutableMap.of();
    }

    List<Path> allUsedPaths = new ArrayList<>();
    for (Path usedClassesMapPath : usedClassesMapPaths) {
      ImmutableMap<Path, Set<Path>> usedClassesMap =
          ObjectMappers.readValue(usedClassesMapPath, new TypeReference<>() {});

      for (Map.Entry<Path, Set<Path>> usedClassesEntry : usedClassesMap.entrySet()) {
        Path usedJarPath = usedClassesEntry.getKey();
        Path usedJarDir = jarToJarDirMap.get(usedJarPath);
        if (usedJarDir == null) {
          allUsedPaths.add(usedJarPath);
        } else {
          for (Path usedClass : usedClassesEntry.getValue()) {
            allUsedPaths.add(usedJarDir.resolve(usedClass));
          }
        }
      }
    }

    if (append && Files.exists(depFileOutput)) {
      Files.readAllLines(depFileOutput).stream().forEach(path -> allUsedPaths.add(Paths.get(path)));
    }

    Files.write(
        depFileOutput,
        allUsedPaths.stream().map(Path::toString).distinct().sorted().collect(Collectors.toList()));
  }

  /**
   * Maps used-classes.json files to a used-jars.json that can be used to remove unused
   * dependencies.
   */
  public static void usedClassesToUsedJars(
      List<Path> usedClassesJsonPaths, Path usedJarsFileOutput, boolean append) throws IOException {
    Map<Path, List<Path>> usedJarsToClasses = new TreeMap<>();

    if (append && Files.exists(usedJarsFileOutput)) {
      Map<Path, List<Path>> existingEntries =
          ObjectMappers.readValue(usedJarsFileOutput, new TypeReference<>() {});
      usedJarsToClasses.putAll(existingEntries);
    }

    // Merge multiple used-classes.json files into a single map of jar to classes
    for (Path usedClassesJsonPath : usedClassesJsonPaths) {
      ImmutableMap<Path, Set<Path>> usedClassesMap =
          ObjectMappers.readValue(usedClassesJsonPath, new TypeReference<>() {});

      usedClassesMap.forEach(
          (usedJarPath, classes) -> {
            usedJarsToClasses.computeIfAbsent(usedJarPath, k -> new ArrayList<>()).addAll(classes);
          });
    }

    ObjectMappers.WRITER.writeValue(
        usedJarsFileOutput.toFile(),
        usedJarsToClasses.entrySet().stream()
            .collect(
                ImmutableSortedMap.toImmutableSortedMap(
                    Comparator.naturalOrder(),
                    Map.Entry::getKey,
                    v -> ImmutableSortedSet.copyOf(v.getValue()))));
  }
}
