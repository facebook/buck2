/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.aapt

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class ResourceSourceMapExecutableMainTest {
  @get:Rule val temporaryFolder = TemporaryFolder()

  @Test
  fun `resource entries preserve qualifiers and overlay priority`() {
    val firstResources = temporaryFolder.newFolder("first-resources").toPath()
    val secondResources = temporaryFolder.newFolder("second-resources").toPath()
    Files.createDirectories(firstResources.resolve("values"))
    Files.createDirectories(firstResources.resolve("layout"))
    Files.createDirectories(secondResources.resolve("values-fr"))
    Files.writeString(
        firstResources.resolve("values/strings.xml"),
        "<resources><string name=\"title\">First</string></resources>",
        StandardCharsets.UTF_8,
    )
    Files.writeString(
        secondResources.resolve("values-fr/strings.xml"),
        "<resources><string name=\"title\">Second</string></resources>",
        StandardCharsets.UTF_8,
    )
    Files.writeString(
        firstResources.resolve("layout/screen.xml"),
        """
        <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android">
          <TextView android:id="@+id/title_view" />
        </LinearLayout>
        """
            .trimIndent(),
        StandardCharsets.UTF_8,
    )

    val resourceDirs = temporaryFolder.newFile("resource-dirs").toPath()
    Files.write(
        resourceDirs,
        listOf(
            "$firstResources\tfirst/BUCK",
            "   ",
            "$secondResources\tsecond/BUCK",
        ),
        StandardCharsets.UTF_8,
    )
    val assetDirs = temporaryFolder.newFile("asset-dirs").toPath()
    val output = temporaryFolder.newFile("source-map").toPath()

    ResourceSourceMapExecutableMain.generate(resourceDirs, assetDirs, output)

    val expectedLines = listOf(
        "R\t0\tid\ttitle_view\t\tfirst/BUCK\tfirst/BUCK",
        "R\t0\tlayout\tscreen\t\tfirst/BUCK\tfirst/BUCK",
        "R\t0\tstring\ttitle\t\tfirst/BUCK\tfirst/BUCK",
        "R\t1\tstring\ttitle\tfr\tsecond/BUCK\tsecond/BUCK",
    )
    assertEquals(
        expectedLines.joinToString(separator = "\n", postfix = "\n"),
        Files.readString(output, StandardCharsets.UTF_8),
    )
  }

  @Test
  fun `entry point writes an empty map when generation fails`() {
    val resourceDirs = temporaryFolder.newFile("resource-dirs").toPath()
    Files.writeString(
        resourceDirs,
        "${temporaryFolder.root.toPath().resolve("missing-resources")}\tmissing/BUCK",
        StandardCharsets.UTF_8,
    )
    val assetDirs = temporaryFolder.newFile("asset-dirs").toPath()
    val output = temporaryFolder.root.toPath().resolve("fallback/source-map")

    ResourceSourceMapExecutableMain.main(
        arrayOf(
            "--resource-dirs",
            resourceDirs.toString(),
            "--asset-dirs",
            assetDirs.toString(),
            "--output=$output",
        ),
    )

    assertEquals(emptyList<String>(), Files.readAllLines(output, StandardCharsets.UTF_8))
  }

  @Test
  fun `generation skips malformed files without discarding valid resources`() {
    val resources = temporaryFolder.newFolder("resources").toPath()
    Files.createDirectories(resources.resolve("values"))
    Files.createDirectories(resources.resolve("layout"))
    Files.writeString(
        resources.resolve("values/strings.xml"),
        "<resources><string name=\"title\">Title</string></resources>",
        StandardCharsets.UTF_8,
    )
    Files.writeString(
        resources.resolve("values/broken.xml"),
        "<resources><string name=\"broken\">",
        StandardCharsets.UTF_8,
    )
    Files.writeString(
        resources.resolve("layout/broken.xml"),
        "<LinearLayout>",
        StandardCharsets.UTF_8,
    )

    val resourceDirs = temporaryFolder.newFile("resource-dirs").toPath()
    Files.writeString(resourceDirs, "$resources\tresources/BUCK", StandardCharsets.UTF_8)
    val assetDirs = temporaryFolder.newFile("asset-dirs").toPath()
    Files.writeString(
        assetDirs,
        "${temporaryFolder.root.toPath().resolve("missing-assets")}\tassets/BUCK",
        StandardCharsets.UTF_8,
    )
    val output = temporaryFolder.newFile("source-map").toPath()

    ResourceSourceMapExecutableMain.generate(resourceDirs, assetDirs, output)

    assertEquals(
        listOf("R\t0\tstring\ttitle\t\tresources/BUCK\tresources/BUCK"),
        Files.readAllLines(output, StandardCharsets.UTF_8),
    )
  }

  @Test
  fun `asset entries preserve paths and overlay priority`() {
    val firstAssets = temporaryFolder.newFolder("first-assets").toPath()
    val secondAssets = temporaryFolder.newFolder("second-assets").toPath()
    Files.createDirectories(firstAssets.resolve("images"))
    Files.createDirectories(secondAssets.resolve("images"))
    val assetPath = "images/icon.webp"
    Files.writeString(firstAssets.resolve(assetPath), "first", StandardCharsets.UTF_8)
    Files.writeString(secondAssets.resolve(assetPath), "second", StandardCharsets.UTF_8)

    val resourceDirs = temporaryFolder.newFile("resource-dirs").toPath()
    val assetDirs = temporaryFolder.newFile("asset-dirs").toPath()
    Files.write(
        assetDirs,
        listOf(
            "$firstAssets\tfirst/BUCK",
            "$secondAssets\tsecond/BUCK",
        ),
        StandardCharsets.UTF_8,
    )
    val output = temporaryFolder.newFile("source-map").toPath()

    ResourceSourceMapExecutableMain.generate(resourceDirs, assetDirs, output)

    assertEquals(
        listOf(
            assetSourceMapRow(0, assetPath, "first/BUCK", "first/BUCK"),
            assetSourceMapRow(1, assetPath, "second/BUCK", "second/BUCK"),
        ),
        Files.readAllLines(output, StandardCharsets.UTF_8),
    )
  }

  @Test
  fun `asset row serialization preserves delimiter characters`() {
    val assetRow =
        ResourceSourceMapFormat.AssetRow(
            2,
            "images/icon\tvariant.webp",
            "asset\nsource",
            "asset\tBUCK",
        )

    assertEquals(assetRow, ResourceSourceMapFormat.parse(assetRow.serialize()))
  }

  private fun assetSourceMapRow(
      priority: Int,
      path: String,
      source: String,
      ownerBuildFile: String,
  ): String = listOf(
      "A",
      priority.toString(),
      encodeField(path),
      encodeField(source),
      encodeField(ownerBuildFile),
  )
      .joinToString("\t")

  private fun encodeField(value: String): String =
      java.util.Base64.getUrlEncoder()
          .withoutPadding()
          .encodeToString(value.toByteArray(StandardCharsets.UTF_8))
}
