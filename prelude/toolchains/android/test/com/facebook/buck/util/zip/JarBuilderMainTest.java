/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.zip;

import static java.nio.file.Files.newOutputStream;
import static java.nio.file.Files.readAllBytes;
import static java.util.Map.entry;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.concat;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Stream;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class JarBuilderMainTest {

  @Parameters
  public static List<TestData> data() {
    final Entry<String, String> fooClass = entry("Foo.class", "class Foo{}");
    final Entry<String, String> mainClass = entry("Main.class", "class Main{}");
    final Entry<String, String> someProperty =
        entry("some.property", "some=" + System.currentTimeMillis());
    final Entry<String, String> someText =
        entry("build.txt", "#buid info" + System.currentTimeMillis());
    return List.of(
        // empty jar
        new TestData(),
        // one jar
        new TestData().jar(List.of(fooClass, mainClass)),
        // few jars
        new TestData().jar(List.of(fooClass)).jar(List.of(someProperty)).jar(List.of(someText)),
        // one resource directory
        new TestData().resource(List.of(someProperty)),
        // few resource directories
        new TestData()
            .resource(List.of(fooClass))
            .resource(List.of(someProperty))
            .resource(List.of(someText)),
        // one jar and one resource directory
        new TestData().jar(List.of(fooClass)).resource(List.of(someText)),
        // few jars and resource directories
        new TestData()
            .jar(List.of(fooClass))
            .jar(List.of(mainClass))
            .resource(List.of(someProperty))
            .resource(List.of(someText)),
        // check if jar override entries
        new TestData()
            .jar(List.of(someProperty))
            .jar(List.of(entry(someProperty.getKey(), someProperty.getValue() + "-override")))
            .expectEntries(List.of(someProperty)),
        // check if jar override from different containers
        new TestData()
            .resource(List.of(someProperty))
            .jar(List.of(entry(someProperty.getKey(), someProperty.getValue() + "-override")))
            .expectEntries(List.of(someProperty)));
  }

  @Rule public TemporaryFolder temporaryFolder = new TemporaryFolder();

  @Parameter public TestData testData;

  @Test
  public void testJarBuilder() throws IOException {
    runAndAssertJarBuilder();
  }

  @Test
  public void testJarBuilderWithConcatJars() throws IOException {
    runAndAssertJarBuilder("--concat-jars");
  }

  private void runAndAssertJarBuilder(Object... extraArgs) throws IOException {

    final TestArgs testArgs = new TestArgs().add(extraArgs);
    final Path outputJarFile = temporaryFolder.newFile("output.jar").toPath();
    final Path entriesToJarFile = temporaryFolder.newFile("entries-to-jar.txt").toPath();

    final List<Path> jars = testData.jars().map(this::randomJar).collect(toList());
    final List<Path> resources = testData.resources().map(this::randomFolder).collect(toList());
    final List<Path> entriesToJar = concat(resources.stream(), jars.stream()).collect(toList());
    final List<Path> manifests =
        testData.manifests().map(this::temporaryManifest).collect(toList());

    Files.write(entriesToJarFile, entriesToJar.stream().map(Object::toString).collect(toList()));

    testArgs.add("--output", outputJarFile).add("--entries-to-jar", entriesToJarFile);
    manifests.forEach(manifestPath -> testArgs.add("--manifest-file", manifestPath));

    final int resultCode = JarBuilderMain.run(testArgs.toArray());

    assertEquals(0, resultCode);
    assertTrue(Files.exists(outputJarFile));

    try (final JarFile jarFile = new JarFile(outputJarFile.toFile())) {
      assertNotNull(jarFile.getJarEntry(JarFile.MANIFEST_NAME));
      if (testData.expectedEntries.isEmpty()) {
        for (final Path path : entriesToJar) {
          assertJarContent(jarFile, path);
        }
      } else {
        assertJarEntries(jarFile, testData.expectedEntries);
      }
    }
  }

  private void assertJarEntries(JarFile testJar, List<Entry<String, String>> asserEntries) {
    for (final Entry<String, String> entry : asserEntries) {
      final byte[] actualBytes = readEntryBytes(testJar, entry.getKey());
      assertEquals(
          entry.getKey(), entry.getValue(), ofNullable(actualBytes).map(String::new).orElse(""));
    }
  }

  private void assertJarContent(JarFile testJar, Path path) throws IOException {
    if (Files.isDirectory(path)) {
      try (final Stream<Path> files = Files.walk(path)) {
        files
            .filter(Predicate.not(Files::isDirectory))
            .forEach(
                file -> {
                  try {
                    final Path entry = path.relativize(file);
                    final byte[] actualBytes = readAllBytes(file);
                    final byte[] expectedBytes = readEntryBytes(testJar, entry.toString());
                    assertArrayEquals(expectedBytes, actualBytes);
                  } catch (IOException e) {
                    throw new RuntimeException(e);
                  }
                });
      }
    } else {
      try (final JarFile jar = new JarFile(path.toFile())) {
        for (final JarEntry entry : Collections.list(jar.entries())) {
          final byte[] actualBytes = readEntryBytes(testJar, entry.getName());
          final byte[] expectedBytes = readEntryBytes(jar, entry.getName());
          assertArrayEquals(entry.getName(), expectedBytes, actualBytes);
        }
      }
    }
  }

  private Path randomFolder(List<Entry<String, String>> entries) {
    try {
      final Path folder = temporaryFolder.newFolder().toPath();
      for (Entry<String, String> entry : entries) {
        final Path entryPath = folder.resolve(entry.getKey());
        Files.createDirectories(entryPath.getParent());
        Files.writeString(entryPath, entry.getValue());
      }
      return folder;
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private Path randomJar(List<Entry<String, String>> entries) {
    try {
      final Path file = temporaryFolder.newFile().toPath();
      try (final JarOutputStream jar = new JarOutputStream(newOutputStream(file))) {
        for (Entry<String, String> entry : entries) {
          jar.putNextEntry(new JarEntry(entry.getKey()));
          jar.write(entry.getValue().getBytes(StandardCharsets.UTF_8));
        }
      }
      return file;
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private Path temporaryManifest(final Manifest manifest) {
    try {
      final Path manifestFile = temporaryFolder.newFile().toPath();
      try (OutputStream out = newOutputStream(manifestFile)) {
        manifest.write(out);
      }
      return manifestFile;
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private byte[] readEntryBytes(JarFile jar, String entry) {
    final JarEntry jarEntry = jar.getJarEntry(entry);
    if (jarEntry == null) {
      return null;
    }
    try (InputStream inputStream = jar.getInputStream(jarEntry)) {
      return inputStream.readAllBytes();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  static class TestArgs {

    private final List<Object> args = new LinkedList<>();

    TestArgs add(Object... args) {
      this.args.addAll(List.of(args));
      return this;
    }

    String[] toArray() {
      return args.stream().map(Objects::toString).toArray(String[]::new);
    }
  }

  public static class TestData {

    private final List<List<Entry<String, String>>> jars = new ArrayList<>();
    private final List<List<Entry<String, String>>> resources = new ArrayList<>();
    private final List<Supplier<Manifest>> manifests = new ArrayList<>();
    private final List<Entry<String, String>> expectedEntries = new ArrayList<>();

    public TestData expectEntries(List<Entry<String, String>> entries) {
      this.expectedEntries.addAll(entries);
      return this;
    }

    public TestData jar(List<Entry<String, String>> entries) {
      this.jars.add(entries);
      return this;
    }

    public TestData resource(List<Entry<String, String>> entries) {
      this.resources.add(entries);
      return this;
    }

    public TestData manifest(Supplier<Manifest> manifest) {
      this.manifests.add(manifest);
      return this;
    }

    public Stream<List<Entry<String, String>>> jars() {
      return jars.stream();
    }

    public Stream<List<Entry<String, String>>> resources() {
      return resources.stream();
    }

    public boolean hasManifests() {
      return !manifests.isEmpty();
    }

    public Stream<Manifest> manifests() {
      return manifests.stream().map(Supplier::get);
    }

    @Override
    public String toString() {
      return "[" + "jars=" + jars + ", resources=" + resources + ']';
    }
  }
}
