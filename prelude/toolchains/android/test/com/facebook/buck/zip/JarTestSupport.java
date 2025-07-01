/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.zip;

import static com.facebook.buck.util.zip.ZipOutputStreams.newJarOutputStream;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.Files.newOutputStream;
import static java.util.Objects.requireNonNull;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.util.zip.CentralDirectoryHeader;
import com.facebook.buck.util.zip.CentralDirectoryReader;
import com.facebook.buck.util.zip.CustomJarOutputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.*;
import java.util.Map.Entry;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;

/** Base class to generate and test JAR files. */
abstract class JarTestSupport {

  public static final String ENTRY_1 = "entry1";
  public static final String ENTRY_2 = "entry2";
  public static final String ENTRY_3 = "entry3";
  public static final String ENTRY_4 = "entry4";
  public static final String ENTRY_5 = "entry5";
  public static final String MANIFEST_DIR = "META-INF/";
  public static final String MANIFEST = MANIFEST_DIR + "MANIFEST.MF";

  static class JarGenerator {

    private final List<Entry<String, byte[]>> entries = new LinkedList<>();
    private final Map<String, String> attributes = new LinkedHashMap<>();

    JarGenerator attr(final String key) {
      return attr(key, key);
    }

    JarGenerator attr(final String key, final String value) {
      attributes.put(key, value);
      return this;
    }

    JarGenerator attrs(final Map<String, String> attrs) {
      attributes.putAll(attrs);
      return this;
    }

    JarGenerator entries(String... keys) {
      Stream.of(keys).forEach(this::entry);
      return this;
    }

    JarGenerator entry(final String key) {
      entry(key, key);
      return this;
    }

    JarGenerator entries(final Map<String, String> entries) {
      return entries(entries.entrySet());
    }

    JarGenerator entries(final Iterable<Entry<String, String>> entries) {
      entries.forEach(e -> entry(e.getKey(), e.getValue()));
      return null;
    }

    JarGenerator entry(final String key, final String value) {
      return entry(key, value.getBytes(UTF_8));
    }

    JarGenerator entry(final String key, final byte[] value) {
      entries.add(Map.entry(key, value));
      return this;
    }

    Path writeTo(final TemporaryPaths temporaryPaths) throws IOException {
      return writeTo(temporaryPaths.newFile().getPath());
    }

    Path writeTo(final Path pathToJar) throws IOException {
      try (final CustomJarOutputStream jar = newJarOutputStream(newOutputStream(pathToJar))) {
        writeTo(jar);
      }
      return pathToJar;
    }

    void writeTo(final CustomJarOutputStream jar) throws IOException {
      if (!attributes.isEmpty()) {
        attributes.forEach(jar.getManifest()::setManifestAttribute);
      }
      for (final Entry<String, byte[]> entry : entries) {
        final JarEntry jarEntry = new JarEntry(entry.getKey());
        final byte[] bytes = entry.getValue();
        jarEntry.setSize(bytes.length);
        jar.putNextEntry(jarEntry);
        jar.write(bytes);
        jar.closeEntry();
      }
    }
  }

  JarGenerator newJar() {
    return new JarGenerator();
  }

  CentralDirectoryHeader readCentralDirectoryHeader(final Path outputJar) throws IOException {
    return new CentralDirectoryReader().readCentralDirectory(outputJar);
  }

  List<String> readJarEntryNames(final Path jarFile) throws IOException {
    try (final JarFile jar = new JarFile(jarFile.toFile())) {
      return jar.stream().map(ZipEntry::getName).collect(toList());
    }
  }

  List<String> readJarEntryValues(final Path jarFile) throws IOException {
    return readJarEntryValues(jarFile, readJarEntryNames(jarFile));
  }

  String readEntryValue(final Path jarFile, final String entry) throws IOException {
    try (final JarFile jar = new JarFile(jarFile.toFile())) {
      return readEntryValue(jar, entry);
    }
  }

  byte[] readEntryBytes(final Path jarFile, final String entry) throws IOException {
    try (final JarFile jar = new JarFile(jarFile.toFile())) {
      return readEntryBytes(jar, entry);
    }
  }

  List<String> readJarEntryValues(final Path jarFile, final List<String> entries)
      throws IOException {
    try (final JarFile jar = new JarFile(jarFile.toFile())) {
      return entries.stream().map(entry -> readEntryValue(jar, entry)).collect(toList());
    }
  }

  Map<String, String> readJarValuesAsMap(final Path jarFile, final Collection<String> entries)
      throws IOException {
    try (final JarFile jar = new JarFile(jarFile.toFile())) {
      return entries.stream().collect(toMap(identity(), entry -> readEntryValue(jar, entry)));
    }
  }

  private byte[] readEntryBytes(final JarFile jar, final String entry) {
    final JarEntry jarEntry = jar.getJarEntry(entry);
    try (final InputStream in = jar.getInputStream(jarEntry)) {
      return requireNonNull(in).readAllBytes();
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }

  private String readEntryValue(final JarFile jar, final String entry) {
    final JarEntry jarEntry = jar.getJarEntry(entry);
    try {
      return getString(jar.getInputStream(jarEntry));
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }

  String getString(final InputStream inputStream) throws IOException {
    if (inputStream == null) {
      return null;
    }
    return new String(inputStream.readAllBytes(), UTF_8);
  }

  byte[] getResourceAsBytes(final ClassLoader classLoader, final String resourcePath) {
    final URL resource = classLoader.getResource(resourcePath);
    if (resource == null) {
      return null;
    }
    try (final InputStream input = resource.openStream()) {
      return input.readAllBytes();
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }

  String getResourceAsString(final ClassLoader classLoader, final String resourcePath) {
    final byte[] bytes = getResourceAsBytes(classLoader, resourcePath);
    if (bytes == null) {
      return null;
    }
    return new String(bytes, UTF_8);
  }

  Manifest getResourceAsManifest(final ClassLoader classLoader, final String resourcePath)
      throws IOException {
    final byte[] bytes = getResourceAsBytes(classLoader, resourcePath);
    if (resourcePath == null) {
      return null;
    }
    return new Manifest(new ByteArrayInputStream(bytes));
  }

  URLClassLoader loadJarAsTestClassLoader(Path outputJar) throws MalformedURLException {
    return new URLClassLoader("test", new URL[] {outputJar.toUri().toURL()}, null) {
      @Override
      public URL getResource(String name) {
        // override default behavior, which will try to load from boot-classloader first
        return findResource(name);
      }
    };
  }
}
