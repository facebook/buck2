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

import com.android.common.ide.common.xml.XmlFormatPreferences;
import com.android.common.ide.common.xml.XmlFormatStyle;
import com.android.common.ide.common.xml.XmlPrettyPrinter;
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
import java.io.StringReader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

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
    xmlText = moveActivityAliasesToEnd(xmlText);

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

  /**
   * At the moment all apps using manifest.py in fbsource already handle how activity-alias are
   * sorted to guarantee they are in the manifest after the activity they target thanks to the post
   * processing happening in @link{<a href="https://fburl.com/code/fsiow1yu">the manifest.py</a>}.
   *
   * <p>In WA this is not the case and we want to guarantee the order is maintained until we are
   * able to update the ManifestMerging plugin to more recent version. This is a link to a @link{<a
   * href="https://fburl.com/workplace/byshefjr">discussion</a>} regarding this specific manifest
   * merging issue.
   *
   * <p>Once the latest version of the ManifestMerger will be updated we can clean up this step and
   * the work is tracked in T239793438
   */
  @VisibleForTesting
  static String moveActivityAliasesToEnd(String xmlContent) {
    try {
      // Parse the XML
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setNamespaceAware(true);
      DocumentBuilder builder = factory.newDocumentBuilder();
      Document doc = builder.parse(new InputSource(new StringReader(xmlContent)));

      // Find the application element
      NodeList applicationNodes = doc.getElementsByTagName("application");
      if (applicationNodes.getLength() == 0) {
        return xmlContent; // No application element, nothing to do
      }

      Element application = (Element) applicationNodes.item(0);
      List<Element> earlyAliases = getEarlyAliases(application);

      if (earlyAliases.isEmpty()) {
        // No early aliases found, so let's just return the original xmlContent
        return xmlContent;
      }

      // Move early aliases to the end
      for (Element alias : earlyAliases) {
        application.removeChild(alias);
        application.appendChild(alias);
      }

      // Use XmlPrettyPrinter to format the output, matching the manifest merger's formatting
      XmlFormatPreferences prefs = XmlFormatPreferences.defaults();
      prefs.removeEmptyLines = true;

      return XmlPrettyPrinter.prettyPrint(
          doc,
          prefs,
          XmlFormatStyle.get(doc),
          null, /* lineSeparator */
          false /* endWithNewline */);

    } catch (Exception e) {
      // If XML processing fails, return original content
      // This ensures the build doesn't break if there's an issue with the reordering
      return xmlContent;
    }
  }

  private static List<Element> getEarlyAliases(Element application) {
    Set<String> activityNames = new HashSet<>();
    List<Element> earlyAliases = new ArrayList<>();

    // Iterate through children to find activities and early aliases
    NodeList children = application.getChildNodes();
    String androidNamespace = "http://schemas.android.com/apk/res/android";
    for (int i = 0; i < children.getLength(); i++) {
      Node node = children.item(i);
      if (node.getNodeType() != Node.ELEMENT_NODE) {
        continue;
      }

      Element element = (Element) node;
      String tagName = element.getTagName();

      if ("activity".equals(tagName)) {
        String name = element.getAttributeNS(androidNamespace, "name");
        if (name != null && !name.isEmpty()) {
          activityNames.add(name);
        }
      } else if ("activity-alias".equals(tagName)) {
        String target = element.getAttributeNS(androidNamespace, "targetActivity");
        if (target != null && !target.isEmpty() && !activityNames.contains(target)) {
          earlyAliases.add(element);
        }
      }
    }
    return earlyAliases;
  }
}
