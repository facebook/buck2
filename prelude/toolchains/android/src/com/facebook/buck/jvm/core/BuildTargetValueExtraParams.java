/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.core;

import static com.google.common.base.Preconditions.checkArgument;

import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.facebook.buck.jvm.cd.serialization.RelPathSerializer;
import com.facebook.buck.util.environment.Platform;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Extra params from {@link BuildTargetValue} needed for Kotlin. */
@BuckStyleValue
public abstract class BuildTargetValueExtraParams {

  public abstract RelPath getCellRelativeBasePath();

  public abstract boolean isFlavored();

  public abstract RelPath getBasePathForBaseName();

  public abstract String getShortNameAndFlavorPostfix();

  public abstract String getShortName();

  public abstract RelPath getConfiguredBuckOut();

  /** Creates {@link BuildTargetValueExtraParams} */
  public static BuildTargetValueExtraParams of(
      RelPath cellRelativeBasePath,
      boolean flavored,
      RelPath basePathForBaseName,
      String shortNameAndFlavorPostfix,
      String shortName,
      RelPath configuredBuckOut) {
    checkArgument(
        shortNameAndFlavorPostfix.startsWith(shortName),
        "shortNameAndFlavorPostfix:%s should start with shortName:%s",
        shortNameAndFlavorPostfix,
        shortName);
    return ImmutableBuildTargetValueExtraParams.ofImpl(
        cellRelativeBasePath,
        flavored,
        basePathForBaseName,
        shortNameAndFlavorPostfix,
        shortName,
        configuredBuckOut);
  }

  public static BuildTargetValueExtraParams of(BuildTargetValue buildTargetValue, RelPath buckOut) {
    // in buck1: cell//base_path:target#flavor vs. in buck2: cell//base_path:target[flavor]
    String buck1Style = "(#(.*))";
    String buck2Style = "(\\[([^\\]]*)\\])";
    Pattern pattern =
        Pattern.compile(String.format("(.*)//([^:]*):([^#\\[]*)(%s|%s)?", buck1Style, buck2Style));
    Matcher matcher = pattern.matcher(buildTargetValue.getFullyQualifiedName());
    if (!matcher.matches()) {
      throw new RuntimeException(
          "Can't reconstruct extra params from fullyQualifiedName: "
              + buildTargetValue.getFullyQualifiedName());
    }
    String basePath = matcher.group(2);
    String targetName = matcher.group(3);
    Optional<String> flavor = Optional.empty();
    if (!Strings.isNullOrEmpty(matcher.group(4))) {
      flavor =
          !Strings.isNullOrEmpty(matcher.group(6))
              ? Optional.of(matcher.group(6))
              : Optional.of(matcher.group(8));
    }
    String shortNameAndFlavorPostfix = targetName + flavor.map("#"::concat).orElse("");
    String buildTargetConfigHash = buildTargetValue.buildTargetConfigHash();
    return of(
        RelPathSerializer.deserialize(basePath),
        flavor.isPresent(),
        RelPath.get(buildTargetConfigHash).resolve(RelPathSerializer.deserialize(basePath)),
        shortNameAndFlavorPostfix,
        targetName,
        buckOut);
  }

  public String getModuleName() {
    String pathSeparator = getCellRelativeBasePath().getFileSystem().getSeparator();
    return getCellRelativeBasePath().toString().replace(pathSeparator, ".") + "." + getShortName();
  }

  public RelPath getKaptAnnotationGenPath() {
    return getGenPath().resolveRel("__kapt_generated__");
  }

  public RelPath getKspAnnotationGenPath() {
    return getGenPath().resolveRel("__ksp_generated__");
  }

  /** Returns annotation path for the given {@code target} and {@code format} */
  public RelPath getAnnotationPath(String format) {
    checkArgument(!format.startsWith("/"), "format string should not start with a slash");
    RelPath configuredBuckOut = getConfiguredBuckOut();
    return getRelativePath(format, configuredBuckOut.resolveRel("annotation"));
  }

  /** Returns `gen` directory path for the given {@code target} */
  public RelPath getGenPath() {
    String format = isFlavored() ? "%s" : "%s__";
    return getGenPath(format);
  }

  /** Returns `gen` directory path for the given {@code target} and {@code format} */
  public RelPath getGenPath(String format) {
    checkArgument(!format.startsWith("/"), "format string should not start with a slash");
    RelPath configuredBuckOut = getConfiguredBuckOut();
    return getRelativePath(format, configuredBuckOut.resolveRel("gen"));
  }

  /** Returns `gen` directory path for the given {@code target} and {@code format} */
  public RelPath getScratchPath(String format) {
    checkArgument(!format.startsWith("/"), "format string should not start with a slash");
    RelPath configuredBuckOut = getConfiguredBuckOut();
    return getRelativePath(format, configuredBuckOut.resolveRel("bin"));
  }

  public RelPath getRelativePath(String format, RelPath directory) {
    return directory.resolve(getBasePath(format));
  }

  public RelPath getBasePath(String format) {
    checkArgument(!format.startsWith("/"), "format string should not start with a slash");
    return getBasePathForBaseName()
        .resolveRel(formatLastSegment(format, getShortNameAndFlavorPostfix()));
  }

  /**
   * Returns a formatted string using the specified format string and a given argument. In case of
   * windows platform back slashes would be replaced with forward slashes before applying string
   * formatting.
   *
   * @param format {@link String#format} string for the path name. It should contain one "%s", which
   *     will be filled in with the rule's short name. It should not start with a slash.
   * @param arg an argument referenced by the format specifier in the format string.
   * @return A formatted string
   */
  private static String formatLastSegment(String format, String arg) {
    Preconditions.checkArgument(
        !format.startsWith("/"), "format string should not start with a slash");

    if (Platform.detect() == Platform.WINDOWS) {
      // TODO(nga): prohibit backslashes in format
      format = format.replace('\\', '/');
    }

    return String.format(format, arg);
  }
}
