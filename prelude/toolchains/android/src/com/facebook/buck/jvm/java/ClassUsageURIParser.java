/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableMap;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

/** Take URI input for class usage data, and output the total class usage as map */
public class ClassUsageURIParser {

  private static final String FILE_SCHEME = "file";
  private static final String JAR_SCHEME = "jar";
  private static final String JIMFS_SCHEME = "jimfs"; // Used in tests

  // Examples: First anonymous class is Foo$1.class. First local class named Bar is Foo$1Bar.class.
  private static final Pattern LOCAL_OR_ANONYMOUS_CLASS = Pattern.compile("^.*\\$\\d.*.class$");

  private final Map<Path, Map<Path, Integer>> resultBuilder = new HashMap<>();

  /**
   * Returns a map from JAR path on disk to .class file paths within the jar for any classes that
   * were used, and the count for how often those classes were read.
   */
  public ImmutableMap<Path, Map<Path, Integer>> getClassUsageMap() {
    return ImmutableMap.copyOf(resultBuilder);
  }

  /** Parse URI input and record it in class usage cache */
  public void parseAndRecordURI(URI classFileJarUri) {
    if (classFileJarUri.getScheme() == null || !classFileJarUri.getScheme().equals(JAR_SCHEME)) {
      // Not in a jar; must not have been built with java_library
      return;
    }

    // The jar: scheme is somewhat underspecified. See the JarURLConnection docs
    // for the closest thing it has to documentation.
    String jarUriSchemeSpecificPart = classFileJarUri.getRawSchemeSpecificPart();
    String[] split = jarUriSchemeSpecificPart.split("!/");
    Preconditions.checkState(split.length == 2);

    if (isLocalOrAnonymousClass(split[1])) {
      // The compiler reads local and anonymous classes because of the naive way in which it
      // completes the enclosing class, but changes to them can't affect compilation of dependent
      // classes so we don't need to consider them "used".
      return;
    }

    URI jarFileUri = URI.create(split[0]);
    Preconditions.checkState(
        jarFileUri.getScheme().equals(FILE_SCHEME)
            || jarFileUri.getScheme().equals(JIMFS_SCHEME)); // jimfs is used in tests
    Path jarFilePath = Paths.get(jarFileUri);

    // Using URI.create here for de-escaping
    Path classPath = Paths.get(URI.create(split[1]).toString());

    Preconditions.checkState(jarFilePath.isAbsolute());
    Preconditions.checkState(!classPath.isAbsolute());

    Map<Path, Integer> classpaths =
        resultBuilder.computeIfAbsent(jarFilePath, _path -> new HashMap<>());
    classpaths.put(classPath, classpaths.getOrDefault(classPath, 0) + 1);
  }

  private boolean isLocalOrAnonymousClass(String className) {
    return LOCAL_OR_ANONYMOUS_CLASS.matcher(className).matches();
  }
}
