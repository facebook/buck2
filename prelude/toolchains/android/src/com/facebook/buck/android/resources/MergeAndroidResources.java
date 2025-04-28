/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.resources;

import com.facebook.buck.android.aapt.RDotTxtEntry;
import com.facebook.buck.android.aapt.RDotTxtEntry.RType;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.util.ThrowingPrintWriter;
import com.facebook.buck.util.json.ObjectMappers;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;
import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class MergeAndroidResources {
  private static final Logger LOG = Logger.get(MergeAndroidResources.class);

  /**
   * Merges text symbols files from {@code aapt} for each of the input {@code android_resource} into
   * a set of resources per R.java package and writes an {@code R.java} file per package under the
   * output directory. Also, if {@code uberRDotTxt} is present, the IDs in the output {@code R.java}
   * file will be taken from the {@code R.txt} file.
   */
  public static void mergeAndroidResources(
      ImmutableList<Path> uberRDotTxt,
      ImmutableMap<Path, String> symbolsFileToRDotJavaPackage,
      ImmutableMap<Path, String> symbolsFileToResourceDeps,
      boolean forceFinalResourceIds,
      EnumSet<RType> bannedDuplicateResourceTypes,
      Optional<Path> duplicateResourceWhitelistPath,
      Optional<String> unionPackage,
      ImmutableList<Path> overrideSymbolsPath,
      Path outputDir,
      Optional<Path> stringsOutputDir,
      Optional<Path> idsOutputDir,
      ImmutableSet<String> referencedResources)
      throws DuplicateResourceException, IOException {
    // In order to convert a symbols file to R.java, all resources of the same type are grouped
    // into a static class of that name. The static class contains static values that correspond
    // to the resource (type, name, value) tuples. See RDotTxtEntry.
    //
    // The first step is to merge symbol files of the same package type and resource type/name.
    // That is, within a package type, each resource type/name pair must be unique. If there are
    // multiple pairs, only one will be written to the R.java file.
    //
    // Because the resulting files do not match their respective resources.arsc, the values are
    // meaningless and do not represent the usable final result.  This is why the R.java file is
    // written without using final so that javac will not inline the values.
    Optional<ImmutableMap<RDotTxtEntry, String>> uberRDotTxtIds;
    if (uberRDotTxt.isEmpty()) {
      uberRDotTxtIds = Optional.empty();
    } else {
      // re-assign Ids
      ImmutableSet.Builder<RDotTxtEntry> uberRdotTxtEntries = ImmutableSet.builder();
      uberRDotTxt.forEach(
          rDot -> {
            try {
              readResources(rDot).forEach(uberRdotTxtEntries::add);
            } catch (IOException e) {
              throw new RuntimeException(e);
            }
          });
      uberRDotTxtIds =
          Optional.of(
              uberRdotTxtEntries.build().stream()
                  .collect(ImmutableMap.toImmutableMap(input -> input, input -> input.idValue)));
    }

    Optional<SetMultimap<String, RDotTxtEntry>> overrideSymbols =
        loadOverrideSymbols(overrideSymbolsPath);

    ImmutableSet<String> duplicateResourceWhitelist =
        (duplicateResourceWhitelistPath.isPresent())
            ? ImmutableSet.copyOf(Files.readAllLines(duplicateResourceWhitelistPath.get()))
            : ImmutableSet.of();

    SortedSetMultimap<String, RDotTxtEntry> rDotJavaPackageToResources =
        sortSymbols(
            symbolsFileToRDotJavaPackage,
            uberRDotTxtIds,
            symbolsFileToResourceDeps,
            overrideSymbols,
            bannedDuplicateResourceTypes,
            duplicateResourceWhitelist);

    ImmutableSet.Builder<String> requiredPackages = ImmutableSet.builder();

    // Create a temporary list as the multimap
    // will be concurrently modified below.
    ArrayList<Entry<String, RDotTxtEntry>> entries =
        new ArrayList<>(rDotJavaPackageToResources.entries());

    requiredPackages.addAll(symbolsFileToRDotJavaPackage.values());

    // If a resource_union_package was specified, copy all resource into that package,
    // unless they are already present.
    if (unionPackage.isPresent()) {
      String unionPackageName = unionPackage.get();
      requiredPackages.add(unionPackageName);

      for (Entry<String, RDotTxtEntry> entry : entries) {
        if (!rDotJavaPackageToResources.containsEntry(unionPackageName, entry.getValue())) {
          rDotJavaPackageToResources.put(unionPackageName, entry.getValue());
        }
      }
    }

    Preconditions.checkState(
        stringsOutputDir.isPresent() == idsOutputDir.isPresent(),
        "Expect either both stringOutputDir and idsOutputDir, or neither");
    boolean hasStringsAndIdsOutputDir = stringsOutputDir.isPresent();

    Set<String> emptyPackages;
    if (hasStringsAndIdsOutputDir) {
      SortedSetMultimap<String, RDotTxtEntry> otherRDotJavaPackageToResources =
          TreeMultimap.create();
      SortedSetMultimap<String, RDotTxtEntry> stringsRDotJavaPackageToResources =
          TreeMultimap.create();
      SortedSetMultimap<String, RDotTxtEntry> idsRDotJavaPackageToResources = TreeMultimap.create();
      for (Entry<String, RDotTxtEntry> entry : rDotJavaPackageToResources.entries()) {
        RType resourceType = entry.getValue().type;
        if (resourceType.equals(RType.STRING)) {
          stringsRDotJavaPackageToResources.put(entry.getKey(), entry.getValue());
        } else if (resourceType.equals(RType.ID)) {
          idsRDotJavaPackageToResources.put(entry.getKey(), entry.getValue());
        } else {
          otherRDotJavaPackageToResources.put(entry.getKey(), entry.getValue());
        }
      }

      writePerPackageRDotJava(
          outputDir, otherRDotJavaPackageToResources, forceFinalResourceIds, referencedResources);
      writePerPackageRDotJava(
          stringsOutputDir.get(),
          stringsRDotJavaPackageToResources,
          forceFinalResourceIds,
          referencedResources);
      writePerPackageRDotJava(
          idsOutputDir.get(),
          idsRDotJavaPackageToResources,
          forceFinalResourceIds,
          referencedResources);
      emptyPackages =
          Sets.difference(requiredPackages.build(), otherRDotJavaPackageToResources.keySet());
    } else {
      Preconditions.checkState(referencedResources.isEmpty());
      writePerPackageRDotJava(
          outputDir, rDotJavaPackageToResources, forceFinalResourceIds, referencedResources);
      emptyPackages =
          Sets.difference(requiredPackages.build(), rDotJavaPackageToResources.keySet());
    }

    if (!emptyPackages.isEmpty()) {
      writeEmptyRDotJavaForPackages(outputDir, emptyPackages);
    }
  }

  /**
   * Read resource IDs from a R.txt file and add them to a list of entries
   *
   * @param rDotTxt the path to the R.txt file to read
   * @return a list of RDotTxtEntry objects read from the file
   * @throws IOException
   */
  private static List<RDotTxtEntry> readResources(Path rDotTxt) throws IOException {
    return Files.readAllLines(rDotTxt).stream()
        .filter(input -> !Strings.isNullOrEmpty(input))
        .map(RDotTxtEntry.TO_ENTRY)
        .collect(Collectors.toList());
  }

  private static Optional<SetMultimap<String, RDotTxtEntry>> loadOverrideSymbols(
      Iterable<Path> paths) throws IOException {
    ImmutableSetMultimap.Builder<String, RDotTxtEntry> symbolsBuilder =
        ImmutableSetMultimap.builder();
    for (Path path : paths) {
      if (!Files.isRegularFile(path)) {
        LOG.info("Override-symbols file %s is not present or not regular.  Skipping.", path);
        continue;
      }
      JsonNode jsonData = ObjectMappers.READER.readTree(ObjectMappers.createParser(path));
      for (String packageName : (Iterable<String>) jsonData::fieldNames) {
        Iterator<JsonNode> rDotTxtLines = jsonData.get(packageName).elements();
        while (rDotTxtLines.hasNext()) {
          String rDotTxtLine = rDotTxtLines.next().asText();
          symbolsBuilder.put(packageName, parseEntryOrThrow(rDotTxtLine));
        }
      }
    }
    return Optional.of(symbolsBuilder.build());
  }

  private static void writeEmptyRDotJavaForPackages(Path outputDir, Set<String> rDotJavaPackages)
      throws IOException {
    for (String rDotJavaPackage : rDotJavaPackages) {
      Path outputFile = getPathToRDotJava(outputDir, rDotJavaPackage);
      Files.createDirectories(outputFile.getParent());
      Files.write(
          outputFile,
          String.format("package %s;\n\npublic class R {}\n", rDotJavaPackage)
              .getBytes(StandardCharsets.UTF_8));
    }
  }

  private static void writePackagePrivateArrayHolderClass(
      Path outputFile,
      String packageName,
      String className,
      String fieldName,
      ImmutableList<Integer> arrayContents)
      throws IOException {
    try (ThrowingPrintWriter writer =
        new ThrowingPrintWriter(new FileOutputStream(outputFile.toFile()))) {
      writer.format("package %s;\n\n", packageName);
      writer.format("final class %s {\n", className);
      writer.format("  static final int[] %s = ", fieldName);
      writer.format("{ %s };\n", Joiner.on(",").join(arrayContents));
      writer.println("}");
    }
  }

  private static void writePerPackageRDotJava(
      Path outputDir,
      SortedSetMultimap<String, RDotTxtEntry> packageToResources,
      boolean forceFinalResourceIds,
      ImmutableSet<String> referencedResources)
      throws IOException {
    Files.createDirectories(outputDir);
    ImmutableList.Builder<Integer> allCustomDrawablesBuilder = ImmutableList.builder();
    ImmutableList.Builder<Integer> allGrayscaleImagesBuilder = ImmutableList.builder();
    for (String rDotJavaPackage : packageToResources.keySet()) {
      Path outputFile = getPathToRDotJava(outputDir, rDotJavaPackage);
      Files.createDirectories(outputFile.getParent());
      try (ThrowingPrintWriter writer =
          new ThrowingPrintWriter(new FileOutputStream(outputFile.toFile()))) {
        writer.format("package %s;\n\n", rDotJavaPackage);
        writer.write("public class R {\n");

        ImmutableList.Builder<Integer> customDrawablesBuilder = ImmutableList.builder();
        ImmutableList.Builder<Integer> grayscaleImagesBuilder = ImmutableList.builder();
        RType lastType = null;

        for (RDotTxtEntry res : packageToResources.get(rDotJavaPackage)) {
          boolean isUsed =
              referencedResources.isEmpty()
                  || referencedResources.contains(rDotJavaPackage + "." + res.name);
          RType type = res.type;
          if (isUsed) {
            if (!type.equals(lastType)) {
              // If the previous type needs to be closed, close it.
              if (lastType != null) {
                writer.println("  }\n");
              }

              // Now start the block for the new type.
              writer.format("  public static class %s {\n", type);
              lastType = type;
            }

            // Write out the resource.
            // Write as an int.
            writer.format(
                "    public static%s%s %s=%s;\n",
                forceFinalResourceIds ? " final " : " ", res.idType, res.name, res.idValue);
          }

          if (type == RType.DRAWABLE && res.customType == RDotTxtEntry.CustomDrawableType.CUSTOM) {
            customDrawablesBuilder.add(Integer.decode(res.idValue));
          } else if (type == RType.DRAWABLE
              && res.customType == RDotTxtEntry.CustomDrawableType.GRAYSCALE_IMAGE) {
            grayscaleImagesBuilder.add(Integer.decode(res.idValue));
          }
        }

        // If some type was written (e.g., the for loop was entered), then the last type needs to be
        // closed.
        if (lastType != null) {
          writer.println("  }\n");
        }

        ImmutableList<Integer> customDrawables = customDrawablesBuilder.build();
        if (customDrawables.size() > 0) {
          Path customDrawablesAuxFile =
              getPathToJavaFile(outputDir, rDotJavaPackage, "RCustom.java");
          writePackagePrivateArrayHolderClass(
              customDrawablesAuxFile,
              rDotJavaPackage,
              "RCustom",
              "custom_drawables",
              customDrawables);
          // Add a new field for the custom drawables.
          writer.format(
              "  public static final int[] custom_drawables = %s.RCustom.custom_drawables;",
              rDotJavaPackage);
          writer.format("\n");
        }

        ImmutableList<Integer> grayscaleImages = grayscaleImagesBuilder.build();
        if (grayscaleImages.size() > 0) {
          Path grayscaleDrawablesAuxFile =
              getPathToJavaFile(outputDir, rDotJavaPackage, "RGrayscale.java");
          writePackagePrivateArrayHolderClass(
              grayscaleDrawablesAuxFile,
              rDotJavaPackage,
              "RGrayscale",
              "grayscale_images",
              grayscaleImages);
          // Add a new field for the grayscale drawables.
          writer.format(
              "  public static final int[] grayscale_images = %s.RGrayscale.grayscale_images;",
              rDotJavaPackage);
          writer.format("\n");
        }

        allCustomDrawablesBuilder.addAll(customDrawables);
        allGrayscaleImagesBuilder.addAll(grayscaleImages);

        // Close the class definition.
        writer.println("}");
      }
    }

    ImmutableList<Integer> allCustomDrawables = allCustomDrawablesBuilder.build();
    ImmutableList<Integer> allGrayscaleImages = allGrayscaleImagesBuilder.build();
    if (!allCustomDrawables.isEmpty() || !allGrayscaleImages.isEmpty()) {
      String drawablesPackage = "com.facebook.buck.android.drawables";
      String drawablesPackageReplaced = drawablesPackage.replace('.', '/');

      // To lessen the likelihood of generating too large static initializer,
      // generate 1 class per array. Further class splitting into chunks holding
      // subarrays could be done if problems continue. Please note that int[]
      // array construction may have ramifications on dead resource elimination.
      String arrayHolderClassCustom = "CustomDrawablesAux";
      Path customOutputFile =
          outputDir
              .resolve(drawablesPackageReplaced)
              .resolve(String.format("%s.java", arrayHolderClassCustom));
      Files.createDirectories(customOutputFile.getParent());
      writePackagePrivateArrayHolderClass(
          customOutputFile,
          drawablesPackage,
          arrayHolderClassCustom,
          "customDrawables",
          ImmutableList.sortedCopyOf(Comparator.naturalOrder(), allCustomDrawables));

      String arrayHolderClassGrayscale = "GrayscaleDrawablesAux";
      Path grayscaleOutputFile =
          outputDir
              .resolve(drawablesPackageReplaced)
              .resolve(String.format("%s.java", arrayHolderClassGrayscale));
      writePackagePrivateArrayHolderClass(
          grayscaleOutputFile,
          drawablesPackage,
          arrayHolderClassGrayscale,
          "grayscaleDrawables",
          ImmutableList.sortedCopyOf(Comparator.naturalOrder(), allGrayscaleImages));

      String drawablesClass = "CustomDrawables";
      Path outputFile =
          outputDir
              .resolve(drawablesPackageReplaced)
              .resolve(String.format("%s.java", drawablesClass));
      try (ThrowingPrintWriter writer =
          new ThrowingPrintWriter(new FileOutputStream(outputFile.toFile()))) {
        writer.format("package %s;\n\n", drawablesPackage);
        writer.format("import java.util.Arrays;\n\n");
        writer.format("public final class %s {\n", drawablesClass);

        writer.format("  private %s() {\n", drawablesClass);
        writer.format("    throw new RuntimeException();\n");
        writer.format("  }\n");
        writer.format("\n");

        writer.format(
            "  private static final int[] customDrawables = %s.%s.customDrawables;",
            drawablesPackage, arrayHolderClassCustom);
        writer.format("\n");

        writer.format(
            "  private static final int[] grayscaleDrawables = %s.%s.grayscaleDrawables;",
            drawablesPackage, arrayHolderClassGrayscale);
        writer.format("\n");

        writer.format("  public static boolean isCustomDrawable(int resourceId) {\n");
        writer.format("    return Arrays.binarySearch(customDrawables, resourceId) >= 0;\n");
        writer.format("  }\n");
        writer.format("\n");

        writer.format("  public static boolean isGrayscaleDrawable(int resourceId) {\n");
        writer.format("    return Arrays.binarySearch(grayscaleDrawables, resourceId) >= 0;\n");
        writer.format("  }\n");
        writer.format("\n");

        writer.format("  public static int[] getCustomDrawableIds() {\n");
        writer.format("    return customDrawables;\n");
        writer.format("  }\n");
        writer.format("\n");

        writer.format("  public static int[] getGrayscaleDrawableIds() {\n");
        writer.format("    return grayscaleDrawables;\n");
        writer.format("  }\n");
        writer.format("\n");

        writer.println("}");
      }
    }
  }

  @VisibleForTesting
  public static SortedSetMultimap<String, RDotTxtEntry> sortSymbols(
      Map<Path, String> symbolsFileToRDotJavaPackage,
      Optional<ImmutableMap<RDotTxtEntry, String>> uberRDotTxtIds,
      ImmutableMap<Path, String> symbolsFileToResourceDeps,
      Optional<SetMultimap<String, RDotTxtEntry>> overrides,
      EnumSet<RType> bannedDuplicateResourceTypes,
      Set<String> duplicateResourceWhitelist)
      throws DuplicateResourceException {
    Map<RDotTxtEntry, String> finalIds = null;
    if (uberRDotTxtIds.isPresent()) {
      finalIds = uberRDotTxtIds.get();
    }

    SortedSetMultimap<String, RDotTxtEntry> rDotJavaPackageToSymbolsFiles = TreeMultimap.create();
    SortedSetMultimap<RDotTxtEntry, Path> bannedDuplicateResourceToSymbolsFiles =
        TreeMultimap.create();

    // Expand the package overrides into per-package self-maps.
    // The self-maps are basically sets, but we need to be able to look up
    // the actual RDotTxtEntry objects to get their ids (which are not included in .equals()).
    Map<String, Map<RDotTxtEntry, RDotTxtEntry>> expandedPackageOverrides = ImmutableMap.of();
    if (overrides.isPresent()) {
      expandedPackageOverrides = new HashMap<>();
      Map<String, Map<RDotTxtEntry, RDotTxtEntry>> ovr = expandedPackageOverrides;
      overrides
          .get()
          .asMap()
          .forEach(
              (pkg, entries) ->
                  ovr.put(pkg, entries.stream().collect(Collectors.toMap(k -> k, v -> v))));
    }

    for (Entry<Path, String> entry : symbolsFileToRDotJavaPackage.entrySet()) {
      Path symbolsFile = entry.getKey();
      // Read the symbols file and parse each line as a Resource.
      List<RDotTxtEntry> linesInSymbolsFile;
      try {
        linesInSymbolsFile =
            Files.readAllLines(symbolsFile).stream()
                .filter(input -> !Strings.isNullOrEmpty(input))
                .map(MergeAndroidResources::parseEntryOrThrow)
                .collect(Collectors.toList());
      } catch (IOException e) {
        throw new RuntimeException(e);
      }

      String packageName = entry.getValue();
      Map<RDotTxtEntry, RDotTxtEntry> packageOverrides =
          expandedPackageOverrides.getOrDefault(packageName, ImmutableMap.of());

      if (!packageOverrides.isEmpty()) {
        // RDotTxtEntry computes hash codes and checks equality only based on type and name,
        // so we can use simple map lookup to find the overridden resource entry.
        for (int i = 0; i < linesInSymbolsFile.size(); i++) {
          RDotTxtEntry mappedEntry = packageOverrides.get(linesInSymbolsFile.get(i));
          if (mappedEntry != null) {
            linesInSymbolsFile.set(i, mappedEntry);
          }
        }
      }

      for (int index = 0; index < linesInSymbolsFile.size(); index++) {
        RDotTxtEntry resource = linesInSymbolsFile.get(index);

        if (uberRDotTxtIds.isPresent()) {
          // TODO(natthu): Make this a hard error once we fix fbandroid and remove all unreferenced
          // non-english strings.
          Objects.requireNonNull(finalIds);
          if (!finalIds.containsKey(resource)) {
            LOG.debug("Cannot find resource '%s' in the uber R.txt.", resource);
            continue;
          }
          resource = resource.copyWithNewIdValue(finalIds.get(resource));
        }

        if (bannedDuplicateResourceTypes.contains(resource.type)) {
          bannedDuplicateResourceToSymbolsFiles.put(resource, symbolsFile);
        }
        rDotJavaPackageToSymbolsFiles.put(packageName, resource);
      }
    }

    // Find any "overridden" resources that were actually new resources and add them.
    Map<RDotTxtEntry, String> finalFinalIds = finalIds;
    overrides.ifPresent(
        ovr ->
            ovr.forEach(
                (pkg, resource) -> {
                  Objects.requireNonNull(finalFinalIds);
                  if (!rDotJavaPackageToSymbolsFiles.containsEntry(pkg, resource)) {
                    String realId = finalFinalIds.get(resource);
                    Preconditions.checkState(
                        realId != null,
                        "ID for resource created by filtering is not present in R.txt: %s",
                        resource);
                    rDotJavaPackageToSymbolsFiles.put(pkg, resource.copyWithNewIdValue(realId));
                  }
                }));

    StringBuilder duplicateResourcesMessage = new StringBuilder();
    for (Entry<RDotTxtEntry, Collection<Path>> resourceAndSymbolsFiles :
        bannedDuplicateResourceToSymbolsFiles.asMap().entrySet()) {
      Collection<Path> paths = resourceAndSymbolsFiles.getValue();
      RDotTxtEntry resource = resourceAndSymbolsFiles.getKey();
      if (paths.size() > 1 && !duplicateIsWhitelisted(resource, duplicateResourceWhitelist)) {
        duplicateResourcesMessage.append(
            String.format(
                "Resource '%s' (%s) is duplicated across: ", resource.name, resource.type));
        duplicateResourcesMessage.append(
            Joiner.on(", ")
                .join(
                    paths.stream()
                        .map(symbolsFileToResourceDeps::get)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList())));
        duplicateResourcesMessage.append("\n");
      }
    }

    if (duplicateResourcesMessage.length() > 0) {
      throw new DuplicateResourceException(duplicateResourcesMessage.toString());
    }

    return rDotJavaPackageToSymbolsFiles;
  }

  private static boolean duplicateIsWhitelisted(RDotTxtEntry resource, Set<String> whitelist) {
    return whitelist.contains(resource.type.toString().toLowerCase() + " " + resource.name);
  }

  private static RDotTxtEntry parseEntryOrThrow(String line) {
    Optional<RDotTxtEntry> parsedEntry = RDotTxtEntry.parse(line);
    Preconditions.checkState(parsedEntry.isPresent(), "Should be able to match '%s'.", line);

    return parsedEntry.get();
  }

  /** Returns {@link Path} to R. java file */
  public static Path getPathToRDotJava(Path outputDir, String rDotJavaPackage) {
    return getPathToJavaFile(outputDir, rDotJavaPackage, "R.java");
  }

  static Path getPathToJavaFile(Path outputDir, String javaPackage, String fileName) {
    return outputDir.resolve(javaPackage.replace('.', '/')).resolve(fileName);
  }

  public static class DuplicateResourceException extends Exception {
    DuplicateResourceException(String messageFormat, Object... args) {
      super(String.format(messageFormat, args));
    }
  }
}
