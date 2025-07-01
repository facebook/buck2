/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.manifest;

import com.android.common.utils.ILogger;
import com.android.manifmerger.ManifestMerger2;
import com.android.manifmerger.MergingReport;
import com.android.manifmerger.PlaceholderHandler;
import com.facebook.buck.android.DefaultAndroidManifestReader;
import com.facebook.buck.android.apkmodule.APKModule;
import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.util.environment.Platform;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import com.google.common.io.Files;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class GenerateManifest {

  public static String generateXml(
      Path skeletonManifestPath,
      String moduleName,
      ImmutableList<Path> libraryManifestPaths,
      ImmutableMap<String, String> placeholders,
      Path outManifestPath,
      Path mergeReportPath,
      ILogger logger)
      throws IOException {
    if (skeletonManifestPath.getNameCount() == 0) {
      throw new HumanReadableException("Skeleton manifest filepath is missing");
    }

    if (outManifestPath.getNameCount() == 0) {
      throw new HumanReadableException("Output Manifest filepath is missing");
    }

    Files.createParentDirs(outManifestPath.toFile());

    List<File> libraryManifestFiles =
        libraryManifestPaths.stream().map(Path::toFile).collect(ImmutableList.toImmutableList());

    MergingReport mergingReport =
        mergeManifests(
            moduleName,
            skeletonManifestPath.toFile(),
            libraryManifestFiles,
            mergeReportPath,
            logger);

    String xmlText = mergingReport.getMergedDocument(MergingReport.MergedManifestKind.MERGED);
    xmlText = replacePlaceholders(xmlText, placeholders);

    if (Platform.detect() == Platform.WINDOWS) {
      // Convert line endings to Lf on Windows.
      xmlText = xmlText.replace("\r\n", "\n");
    }

    return xmlText;
  }

  public static String replaceApplicationIdPlaceholders(
      String currentManifest, boolean verifyNoPlaceholdersLeft) throws IOException {
    // Library manifests can set the applicationId placeholder in the expectation that it will
    // be replaced by the actual applicationId in the final manifest. The applicationId is
    // the "package" attribute in the final manifest, so we extract that here and then use it.
    // See https://developer.android.com/studio/build/configure-app-module#set-application-id
    final String applicationPackage =
        DefaultAndroidManifestReader.forString(currentManifest).getPackage();

    String manifest =
        replacePlaceholders(
            currentManifest,
            ImmutableMap.of(PlaceholderHandler.APPLICATION_ID, applicationPackage));

    if (verifyNoPlaceholdersLeft) {
      verifyAllPlaceholdersAreReplacedInManifestOrThrow(manifest);
    }

    return manifest;
  }

  private static MergingReport mergeManifests(
      String moduleName,
      File mainManifestFile,
      List<File> libraryManifestFiles,
      Path mergeReportPath,
      ILogger logger) {
    try {
      ManifestMerger2.Invoker<?> manifestInvoker =
          ManifestMerger2.newMerger(
              mainManifestFile, logger, ManifestMerger2.MergeType.APPLICATION);
      if (!APKModule.isRootModule(moduleName)) {
        manifestInvoker.setPlaceHolderValue("split", moduleName);
      } else {
        manifestInvoker.withFeatures(ManifestMerger2.Invoker.Feature.NO_PLACEHOLDER_REPLACEMENT);
      }

      MergingReport mergingReport =
          manifestInvoker
              .withFeatures(
                  ManifestMerger2.Invoker.Feature.REMOVE_TOOLS_DECLARATIONS,
                  ManifestMerger2.Invoker.Feature.SKIP_BLAME)
              .addLibraryManifests(Iterables.toArray(libraryManifestFiles, File.class))
              .setMergeReportFile(mergeReportPath.toFile())
              .merge();
      if (mergingReport.getResult().isError()) {
        for (MergingReport.Record record : mergingReport.getLoggingRecords()) {
          logger.error(null, record.toString());
        }
        throw new HumanReadableException("Error generating manifest file");
      }

      return mergingReport;
    } catch (ManifestMerger2.MergeFailureException e) {
      throw new HumanReadableException(
          e.getCause(), "Error generating manifest file: %s", e.getMessage());
    }
  }

  @VisibleForTesting
  static String replacePlaceholders(String content, ImmutableMap<String, String> placeholders) {
    Iterable<String> escaped = Iterables.transform(placeholders.keySet(), Pattern::quote);

    Joiner joiner = Joiner.on("|");
    String patternString =
        Pattern.quote("${") + "(" + joiner.join(escaped) + ")" + Pattern.quote("}");
    Pattern pattern = Pattern.compile(patternString);
    Matcher matcher = pattern.matcher(content);

    StringBuffer sb = new StringBuffer();
    while (matcher.find()) {
      final String value = placeholders.get(matcher.group(1));
      // There are instances where the value of a placeholder points to another placeholder, having
      // the form of `${key}`.
      // This clashes with the implementation details of `Matcher.appendReplacement`, which uses the
      // same form to identify what to replace. Escaping `$` prevents this conflict from happening.
      final String escapedValue = value.replace("$", "\\$");
      matcher.appendReplacement(sb, escapedValue);
    }
    matcher.appendTail(sb);
    return sb.toString();
  }

  private static void verifyAllPlaceholdersAreReplacedInManifestOrThrow(String content) {
    String contentWithoutComments = content.replaceAll("<!--[\\s\\S\\n]*?-->", "");
    Pattern pattern = Pattern.compile(Pattern.quote("${") + "(.*?)" + Pattern.quote("}"));
    Matcher matcher = pattern.matcher(contentWithoutComments);

    Set<String> nonHandledPlaceholders = new HashSet<>();
    while (matcher.find()) {
      nonHandledPlaceholders.add(matcher.group(1));
    }

    if (!nonHandledPlaceholders.isEmpty()) {
      throw new HumanReadableException(
          "Not handled placeholders (%s) in manifest: %s", nonHandledPlaceholders, content);
    }
  }
}
