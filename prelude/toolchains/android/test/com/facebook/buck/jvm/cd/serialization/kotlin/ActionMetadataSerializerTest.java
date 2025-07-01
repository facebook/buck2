/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.kotlin;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

import com.facebook.buck.cd.model.kotlin.Metadata;
import com.facebook.buck.jvm.java.ActionMetadata;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.Test;

public class ActionMetadataSerializerTest {

  @Test
  public void testSerialization() {
    ActionMetadata actionMetadata =
        new ActionMetadata(Path.of(""), createPreviousDigest(), createCurrentDigest());
    com.facebook.buck.cd.model.kotlin.ActionMetadata expectedActionMetadata =
        createActionMetaData(actionMetadata.getPreviousDigest(), actionMetadata.getCurrentDigest());

    com.facebook.buck.cd.model.kotlin.ActionMetadata resultActionMetadata =
        ActionMetadataSerializer.serialize(actionMetadata);

    assertThat(resultActionMetadata, equalTo(expectedActionMetadata));
  }

  @Test
  public void testDeserialization() {
    Map<Path, String> previousDigest = createPreviousDigest();
    Map<Path, String> currentDigest = createCurrentDigest();
    com.facebook.buck.cd.model.kotlin.ActionMetadata actionMetadata =
        createActionMetaData(previousDigest, currentDigest);
    ActionMetadata expectedActionMetadata =
        new ActionMetadata(Path.of(""), previousDigest, currentDigest);

    ActionMetadata resultActionMetadata =
        ActionMetadataSerializer.deserialize(Path.of(""), actionMetadata);

    assertThat(resultActionMetadata, equalTo(expectedActionMetadata));
  }

  private static com.facebook.buck.cd.model.kotlin.ActionMetadata createActionMetaData(
      Map<Path, String> previousDigest, Map<Path, String> currentDigest) {
    Metadata.Builder expectedPreviousMetadataBuilder = Metadata.newBuilder();
    expectedPreviousMetadataBuilder.addAllDigests(
        previousDigest.entrySet().stream()
            .map(DigestSerializer::serialize)
            .collect(Collectors.toList()));

    Metadata.Builder expectedCurrentMetadataBuilder = Metadata.newBuilder();
    expectedCurrentMetadataBuilder.addAllDigests(
        currentDigest.entrySet().stream()
            .map(DigestSerializer::serialize)
            .collect(Collectors.toList()));

    return com.facebook.buck.cd.model.kotlin.ActionMetadata.newBuilder()
        .setPreviousMetadata(expectedPreviousMetadataBuilder)
        .setCurrentMetadata(expectedCurrentMetadataBuilder)
        .build();
  }

  private static Map<Path, String> createPreviousDigest() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(
        Paths.get("xplat/toolchains/android/sdk/third-party/java/kotlin/2.0.0/trove4j.jar"),
        "3eb9d0002a9e400709df1aec6e4f4d148ffc32beba871b55aad26287e5ff7606:572985");
    previousDigest.put(
        Paths.get("xplat/toolchains/android/sdk/third-party/java/kotlin/2.0.0/kotlin-stdlib.jar"),
        "cfb3956679c288345070ac45b032f1998ef4b76475560804f9c6f32c9b5fc5c4:1729731");
    previousDigest.put(
        Paths.get(
            "xplat/toolchains/android/sdk/third-party/java/kotlin/2.0.0/kotlin-script-runtime.jar"),
        "6c42020b21b0f11ae847392de86c304bbcd5d3b445914d43514052c15c170473:43404");
    return previousDigest;
  }

  private static Map<Path, String> createCurrentDigest() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(
        Paths.get("xplat/toolchains/android/sdk/third-party/java/kotlin/2.0.0/trove4j.jar"),
        "1191f75f1a9440a13b88ab83c8f1614ed7ba70b2bdca5d7266c154fa292f6630:3056424");
    previousDigest.put(
        Paths.get("xplat/toolchains/android/sdk/third-party/java/kotlin/2.0.0/kotlin-compiler.jar"),
        "cfb3956679c288345070ac45b032f1998ef4b76475560804f9c6f32c9b5fc5c4:1729731");
    previousDigest.put(
        Paths.get(
            "xplat/toolchains/android/sdk/third-party/java/kotlin/2.0.0/kotlin-script-runtime.jar"),
        "6c42020b21b0f11ae847392de86c304bbcd5d3b445914d43514052c15c170473:43404");
    return previousDigest;
  }
}
