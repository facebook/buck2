/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.aapt;

import static com.facebook.buck.android.aapt.RDotTxtEntryUtil.FakeEntry;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.facebook.buck.android.aapt.MiniAapt.ResourceParseException;
import com.facebook.buck.android.aapt.RDotTxtEntry.CustomDrawableType;
import com.facebook.buck.android.aapt.RDotTxtEntry.IdType;
import com.facebook.buck.android.aapt.RDotTxtEntry.RType;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.testutil.TemporaryPaths;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.xml.xpath.XPathExpressionException;
import org.hamcrest.core.IsEqual;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class MiniAaptTest {

  private static final ImmutableList<String> RESOURCES =
      ImmutableList.<String>builder()
          .add(
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
              "<LinearLayout>",
              "<Button android:id=\"@+id/button1\" ",
              "android:layout_toLeftOf=\"@id/button2\" ",
              "android:text=\"@string/text\" />",
              "<Button android:id=\"@+id/button3\" ",
              "style:attribute=\"@style/Buck.Theme\" ",
              "android:background=\"@drawable/some_image\" />",
              "<TextView tools:showIn=\"@layout/some_layout\" ",
              "android:id=\"@id/android:empty\" ",
              "android:background=\"?attr/some_attr\"/>",
              "</LinearLayout>")
          .build();

  @Rule public TemporaryPaths tmpFolder = new TemporaryPaths();

  @Rule public ExpectedException thrown = ExpectedException.none();

  private static Set<RDotTxtEntry> createTestingFakes(
      Set<RDotTxtEntry> entries, Function<RDotTxtEntry, RDotTxtEntry> converter) {
    return entries.stream().map(converter).collect(Collectors.toSet());
  }

  private static Set<RDotTxtEntry> createTestingFakes(Set<RDotTxtEntry> entries) {
    return createTestingFakes(entries, RDotTxtEntryUtil::matchDefault);
  }

  private static Set<RDotTxtEntry> createTestingFakesWithParents(Set<RDotTxtEntry> entries) {
    return createTestingFakes(entries, RDotTxtEntryUtil::matchParent);
  }

  private static Set<RDotTxtEntry> createTestingFakesWithCustomDrawables(
      Set<RDotTxtEntry> entries) {
    return createTestingFakes(entries, RDotTxtEntryUtil::matchCustomDrawables);
  }

  @Before
  public void setUp() {}

  @Test
  public void testFindingResourceIdsInXml()
      throws IOException, XPathExpressionException, ResourceParseException {
    ProjectFilesystemUtils.writeLinesToPath(
        tmpFolder.getRoot(), RESOURCES, Paths.get("resource.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    ImmutableSet.Builder<RDotTxtEntry> references = ImmutableSet.builder();
    aapt.processXmlFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("resource.xml")),
        references);

    Set<RDotTxtEntry> definitions = aapt.getResourceCollector().getResources();

    assertEquals(
        createTestingFakes(definitions),
        ImmutableSet.of(
            FakeEntry.create(IdType.INT, RType.ID, "button1"),
            FakeEntry.create(IdType.INT, RType.ID, "button3")));

    assertEquals(
        createTestingFakes(references.build()),
        ImmutableSet.of(
            FakeEntry.create(IdType.INT, RType.DRAWABLE, "some_image"),
            FakeEntry.create(IdType.INT, RType.STRING, "text"),
            FakeEntry.create(IdType.INT, RType.STYLE, "Buck_Theme"),
            FakeEntry.create(IdType.INT, RType.ID, "button2"),
            FakeEntry.create(IdType.INT, RType.ATTR, "some_attr")));
  }

  @Test
  public void testParsingFilesUnderValuesDirectory() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<string name=\"hello\">Hello, <xliff:g id=\"name\">%s</xliff:g>!</string>",
                "<plurals name=\"people\">",
                "   <item quantity=\"zero\">ignore1</item>",
                "   <item quantity=\"many\">ignore2</item>",
                "</plurals>",
                "<skip />",
                "<integer name=\"number\">100</integer>",
                "<dimen name=\"dimension\">100sp</dimen>",
                "<declare-styleable name=\"MyNiceView\">",
                "   <attr name=\"titleText\" />",
                "   <attr name=\"subtitleText\" format=\"string\" />",
                "   <attr name=\"complexAttr\">",
                "       <enum name=\"shouldBeIgnored\" value=\"0\" />",
                "       <enum name=\"alsoIgnore\" value=\"1\" />",
                "       <flag name=\"uselessFlag\" value=\"0x00\" />",
                "   </attr>",
                "   <attr name=\"android:layout_gravity\" />",
                "   <item name=\"should_be_ignored\" />",
                "</declare-styleable>",
                "<eat-comment />",
                "<item type=\"id\" name=\"some_id\" />",
                "<style name=\"Widget.Theme\">",
                "  <item name=\"ignoreMe\" />",
                "</style>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("values.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("values.xml")));

    Set<RDotTxtEntry> definitions = aapt.getResourceCollector().getResources();

    assertEquals(
        createTestingFakes(definitions),
        ImmutableSet.of(
            FakeEntry.create(IdType.INT, RType.STRING, "hello"),
            FakeEntry.create(IdType.INT, RType.PLURALS, "people"),
            FakeEntry.create(IdType.INT, RType.INTEGER, "number"),
            FakeEntry.create(IdType.INT, RType.DIMEN, "dimension"),
            FakeEntry.create(IdType.INT_ARRAY, RType.STYLEABLE, "MyNiceView"),
            FakeEntry.create(IdType.INT, RType.STYLEABLE, "MyNiceView_titleText"),
            FakeEntry.create(IdType.INT, RType.STYLEABLE, "MyNiceView_subtitleText"),
            FakeEntry.create(IdType.INT, RType.STYLEABLE, "MyNiceView_complexAttr"),
            FakeEntry.create(IdType.INT, RType.STYLEABLE, "MyNiceView_android_layout_gravity"),
            FakeEntry.create(IdType.INT, RType.ATTR, "titleText"),
            FakeEntry.create(IdType.INT, RType.ATTR, "subtitleText"),
            FakeEntry.create(IdType.INT, RType.ATTR, "complexAttr"),
            FakeEntry.create(IdType.INT, RType.ID, "some_id"),
            FakeEntry.create(IdType.INT, RType.STYLE, "Widget_Theme")));

    boolean foundElement = false;
    for (RDotTxtEntry definition : definitions) {
      if (definition.name.equals("MyNiceView")) {
        assertEquals("{ 0x7f060001,0x7f060002,0x7f060003,0x7f060004 }", definition.idValue);
        foundElement = true;
      }
    }
    assertTrue(foundElement);
  }

  @Test
  public void testParentIsSet() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
                    + "<resources>\n"
                    + "    <attr name=\"justAttr\"/>\n"
                    + "    <declare-styleable name=\"MyLayout\">\n"
                    + "        <attr name=\"myAttr\"/>\n"
                    + "        <attr name=\"myAttr2\"/>\n"
                    + "    </declare-styleable>\n"
                    + "    <declare-styleable name=\"MyLayout_Layout\">\n"
                    + "        <attr name=\"android:text\"/>\n"
                    + "        <attr name=\"android:color\"/>\n"
                    + "    </declare-styleable>\n"
                    + "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("values.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("values.xml")));

    Set<RDotTxtEntry> definitions = aapt.getResourceCollector().getResources();

    assertEquals(
        createTestingFakesWithParents(definitions),
        ImmutableSet.of(
            FakeEntry.create(IdType.INT, RType.ATTR, "justAttr"),
            FakeEntry.create(IdType.INT_ARRAY, RType.STYLEABLE, "MyLayout"),
            FakeEntry.createWithParent(IdType.INT, RType.STYLEABLE, "MyLayout_myAttr", "MyLayout"),
            FakeEntry.createWithParent(IdType.INT, RType.STYLEABLE, "MyLayout_myAttr2", "MyLayout"),
            FakeEntry.create(IdType.INT_ARRAY, RType.STYLEABLE, "MyLayout_Layout"),
            FakeEntry.createWithParent(
                IdType.INT, RType.STYLEABLE, "MyLayout_Layout_android_text", "MyLayout_Layout"),
            FakeEntry.createWithParent(
                IdType.INT, RType.STYLEABLE, "MyLayout_Layout_android_color", "MyLayout_Layout"),
            FakeEntry.create(IdType.INT, RType.ATTR, "myAttr"),
            FakeEntry.create(IdType.INT, RType.ATTR, "myAttr2")));
  }

  @Test
  public void testParsingValuesExcludedFromResMap() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources exclude-from-buck-resource-map=\"true\">",
                "<string name=\"hello\">Hello, <xliff:g id=\"name\">%s</xliff:g>!</string>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("values.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("values.xml")));

    Set<RDotTxtEntry> definitions = aapt.getResourceCollector().getResources();

    assertTrue(definitions.isEmpty());
  }

  @Test
  public void testParsingValuesNotExcludedFromResMap() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources exclude-from-buck-resource-map=\"false\">",
                "<string name=\"hello\">Hello, <xliff:g id=\"name\">%s</xliff:g>!</string>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("values.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("values.xml")));

    Set<RDotTxtEntry> definitions = aapt.getResourceCollector().getResources();

    assertEquals(
        createTestingFakes(definitions),
        ImmutableSet.<RDotTxtEntry>of(FakeEntry.create(IdType.INT, RType.STRING, "hello")));
  }

  @Test
  public void testParsingAndroidDrawables() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<bitmap xmlns:android=\"http://schemas.android.com/apk/res/android\">",
                "  xmlns:fbui=\"http://schemas.android.com/apk/res-auto\"",
                "  android:src=\"@drawable/other_bitmap\"",
                "  >",
                "</bitmap>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(
        tmpFolder.getRoot(), lines, Paths.get("android_drawable.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processDrawables(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("android_drawable.xml")));

    Set<RDotTxtEntry> definitions = aapt.getResourceCollector().getResources();

    assertEquals(
        createTestingFakes(definitions),
        ImmutableSet.<RDotTxtEntry>of(
            FakeEntry.create(IdType.INT, RType.DRAWABLE, "android_drawable")));
  }

  @Test
  public void testParsingCustomDrawables() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<app-network xmlns:android=\"http://schemas.android.com/apk/res/android\">",
                "  xmlns:fbui=\"http://schemas.android.com/apk/res-auto\"",
                "  fbui:imageUri=\"http://facebook.com\"",
                "  android:width=\"128px\"",
                "  android:height=\"128px\"",
                "  fbui:density=\"160\"",
                "  >",
                "</app-network>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(
        tmpFolder.getRoot(), lines, Paths.get("custom_drawable.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processDrawables(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("custom_drawable.xml")));

    Set<RDotTxtEntry> definitions = aapt.getResourceCollector().getResources();

    assertEquals(
        createTestingFakesWithCustomDrawables(definitions),
        ImmutableSet.<RDotTxtEntry>of(
            FakeEntry.createWithCustomDrawable(
                IdType.INT, RType.DRAWABLE, "custom_drawable", CustomDrawableType.CUSTOM)));
  }

  private void testParsingGrayscaleImageImpl(String normalFilename, String grayscaleFilename)
      throws IOException, ResourceParseException {
    ImmutableList<String> lines = ImmutableList.<String>builder().add("").build();
    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get(normalFilename));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processDrawables(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get(grayscaleFilename)));

    Set<RDotTxtEntry> definitions = aapt.getResourceCollector().getResources();

    assertThat(
        createTestingFakesWithCustomDrawables(definitions),
        IsEqual.equalToObject(
            ImmutableSet.<RDotTxtEntry>of(
                FakeEntry.createWithCustomDrawable(
                    IdType.INT,
                    RType.DRAWABLE,
                    "fbui_tomato",
                    CustomDrawableType.GRAYSCALE_IMAGE))));
  }

  @Test
  public void testParsingGrayscaleImage() throws IOException, ResourceParseException {
    testParsingGrayscaleImageImpl("fbui_tomato.png", "fbui_tomato.g.png");
    testParsingGrayscaleImageImpl("fbui_tomato.png", "fbui_tomato_g.png");
  }

  @Test(expected = ResourceParseException.class)
  public void testInvalidResourceType() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<resourcetype name=\"number\">100</resourcetype>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("values.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("values.xml")));
  }

  @Test(expected = ResourceParseException.class)
  public void testInvalidItemResource() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<item name=\"number\">100</item>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("values.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("values.xml")));
  }

  @Test
  public void testInvalidDefinition() throws XPathExpressionException, IOException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<LinearLayout>",
                "<Button android:id=\"@+string/button1\" ",
                "android:layout_toLeftOf=\"@id/button2\" ",
                "android:text=\"@string/text\" />",
                "</LinearLayout>")
            .build();

    Path resource = Paths.get("resource.xml");
    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, resource);

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    try {
      aapt.processXmlFile(
          ProjectFilesystemUtils.getPathForRelativePath(tmpFolder.getRoot(), resource),
          ImmutableSet.builder());
      fail("MiniAapt should throw parsing '@+string/button1'");
    } catch (ResourceParseException e) {
      assertThat(e.getMessage(), containsString("Invalid definition of a resource"));
    }
  }

  @Test
  public void testInvalidReference() throws IOException, XPathExpressionException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<LinearLayout>",
                "<Button android:id=\"@+id/button1\" ",
                "android:layout_toLeftOf=\"@someresource/button2\" ",
                "android:text=\"@string/text\" />",
                "</LinearLayout>")
            .build();

    Path resource = Paths.get("resource.xml");
    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, resource);

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    try {
      aapt.processXmlFile(
          ProjectFilesystemUtils.getPathForRelativePath(tmpFolder.getRoot(), resource),
          ImmutableSet.builder());
      fail("MiniAapt should throw parsing '@someresource/button2'");
    } catch (ResourceParseException e) {
      assertThat(e.getMessage(), containsString("Invalid reference '@someresource/button2'"));
    }
  }

  @Test
  public void testMissingNameAttribute() throws IOException, ResourceParseException {
    thrown.expect(ResourceParseException.class);
    thrown.expectMessage("Error: expected a 'name' attribute in node 'string' with value 'Howdy!'");

    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<string notname=\"hello\">Howdy!</string>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("values.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("values.xml")));
  }

  @Test
  public void testVerifyReferences()
      throws IOException, XPathExpressionException, ResourceParseException {
    ProjectFilesystemUtils.writeLinesToPath(
        tmpFolder.getRoot(), RESOURCES, Paths.get("resource.xml"));

    ImmutableList<String> rDotTxt =
        ImmutableList.of(
            "int string text 0x07010001",
            "int style Buck_Theme 0x07020001",
            "int id button2 0x07030001",
            "int attr attribute 0x07040001");

    RelPath depRTxt = RelPath.get("dep/R.txt");
    ProjectFilesystemUtils.createParentDirs(tmpFolder.getRoot(), depRTxt.getPath());
    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), rDotTxt, depRTxt.getPath());

    MiniAapt aapt =
        new MiniAapt(
            ImmutableSet.of(
                ProjectFilesystemUtils.getPathForRelativePath(tmpFolder.getRoot(), depRTxt)));
    ImmutableSet.Builder<RDotTxtEntry> references = ImmutableSet.builder();
    aapt.processXmlFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("resource.xml")),
        references);

    Set<RDotTxtEntry> missing = aapt.verifyReferences(references.build());

    assertEquals(
        ImmutableSet.<RDotTxtEntry>of(
            FakeEntry.create(IdType.INT, RType.DRAWABLE, "some_image"),
            FakeEntry.create(IdType.INT, RType.ATTR, "some_attr")),
        createTestingFakes(missing));
  }

  @Test
  public void testInvalidNodeId()
      throws IOException, XPathExpressionException, ResourceParseException {
    thrown.expect(ResourceParseException.class);
    thrown.expectMessage("Invalid definition of a resource: '@button2'");

    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<LinearLayout>",
                "<Button android:id=\"@+id/button1\" ",
                "android:layout_toLeftOf=\"@button2\" />",
                "</LinearLayout>")
            .build();

    Path resource = Paths.get("resource.xml");
    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, resource);

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());
    aapt.processXmlFile(
        ProjectFilesystemUtils.getPathForRelativePath(tmpFolder.getRoot(), resource),
        ImmutableSet.builder());
  }

  @Test
  public void testProcessNonValuesFiles() throws IOException, ResourceParseException {
    ProjectFilesystemUtils.createParentDirs(
        tmpFolder.getRoot(), Paths.get("res/drawable/icon.png"));
    Files.createFile(tmpFolder.getRoot().resolve("res/drawable/icon.png").getPath());
    ProjectFilesystemUtils.createParentDirs(
        tmpFolder.getRoot(), Paths.get("res/drawable-ldpi/nine_patch.9.png"));
    Files.createFile(tmpFolder.getRoot().resolve("res/drawable-ldpi/nine_patch.9.png").getPath());
    ProjectFilesystemUtils.createParentDirs(
        tmpFolder.getRoot(), Paths.get("res/transition-v19/some_transition.xml"));
    Files.createFile(
        tmpFolder.getRoot().resolve("res/transition-v19/some_transition.xml").getPath());

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    aapt.processNonValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(tmpFolder.getRoot(), "res/drawable/icon.png")
            .getPath(),
        "drawable");
    aapt.processNonValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
                tmpFolder.getRoot(), "res/drawable/drawable-ldpi/nine_patch.9.png")
            .getPath(),
        "drawable-ldpi");
    aapt.processNonValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
                tmpFolder.getRoot(), "res/transition-v19/some_transition.xml")
            .getPath(),
        "transition-v19");

    assertEquals(
        ImmutableSet.<RDotTxtEntry>of(
            FakeEntry.create(IdType.INT, RType.DRAWABLE, "icon"),
            FakeEntry.create(IdType.INT, RType.DRAWABLE, "nine_patch"),
            FakeEntry.create(IdType.INT, RType.TRANSITION, "some_transition")),
        createTestingFakes(aapt.getResourceCollector().getResources()));
  }

  @Test
  public void testIgnoredFiles() throws IOException {
    ProjectFilesystemUtils.createParentDirs(
        tmpFolder.getRoot(), Paths.get("res/drawable/another_icon.png.orig"));
    Files.createFile(tmpFolder.getRoot().resolve("res/drawable/another_icon.png.orig").getPath());
    assertTrue(
        MiniAapt.shouldIgnoreFile(
            ProjectFilesystemUtils.getPathForRelativePath(
                tmpFolder.getRoot(), Paths.get("res/drawable/another_icon.png.orig"))));

    ProjectFilesystemUtils.createParentDirs(
        tmpFolder.getRoot(), Paths.get("res/drawable/drawable-ldpi/.DS_Store"));
    Files.createFile(tmpFolder.getRoot().resolve("res/drawable/drawable-ldpi/.DS_Store").getPath());
    assertTrue(
        MiniAapt.shouldIgnoreFile(
            ProjectFilesystemUtils.getPathForRelativePath(
                tmpFolder.getRoot(), Paths.get("res/drawable/drawable-ldpi/.DS_Store"))));
  }

  @Test
  public void testDotSeparatedResourceNames()
      throws IOException, XPathExpressionException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<LinearLayout>",
                "<Button android:id=\"@+id/button1\" ",
                "android:text=\"@string/com.buckbuild.taskname\" />",
                "</LinearLayout>")
            .build();

    Path resource = Paths.get("resource.xml");
    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, resource);

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    ImmutableSet.Builder<RDotTxtEntry> references = ImmutableSet.builder();
    aapt.processXmlFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("resource.xml")),
        references);

    assertEquals(
        createTestingFakes(references.build()),
        ImmutableSet.<RDotTxtEntry>of(
            FakeEntry.create(IdType.INT, RType.STRING, "com_buckbuild_taskname")));
  }

  @Test
  public void ignoresValidPublicResourceType() throws IOException, ResourceParseException {
    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<public name=\"some_resource_name\" type=\"string\"/>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("public.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("public.xml")));
  }

  @Test
  public void invalidPublicResourceWithNoName() throws IOException, ResourceParseException {
    thrown.expect(ResourceParseException.class);
    thrown.expectMessage(
        "Error parsing file 'public.xml', expected a 'name' attribute in \n" + "'[public: null]'");

    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<public type=\"string\"/>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("public.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("public.xml")));
  }

  @Test
  public void invalidPublicResourceWithEmptyName() throws IOException, ResourceParseException {
    thrown.expect(ResourceParseException.class);
    thrown.expectMessage(
        "Error parsing file 'public.xml', expected a 'name' attribute in \n" + "'[public: null]'");

    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<public name=\"\" type=\"string\"/>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("public.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("public.xml")));
  }

  @Test
  public void invalidPublicResourceWithNoType() throws IOException, ResourceParseException {
    thrown.expect(ResourceParseException.class);
    thrown.expectMessage(
        "Error parsing file 'public.xml', expected a 'type' attribute in: \n" + "'[public: null]'");

    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<public name=\"some_resource_name\"/>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("public.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("public.xml")));
  }

  @Test
  public void invalidPublicResourceWithEmptyType() throws IOException, ResourceParseException {
    thrown.expect(ResourceParseException.class);
    thrown.expectMessage(
        "Error parsing file 'public.xml', expected a 'type' attribute in: \n" + "'[public: null]'");

    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<public name=\"some_resource_name\" type=\"\"/>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("public.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("public.xml")));
  }

  @Test
  public void invalidPublicResourceWithUnknownType() throws IOException, ResourceParseException {
    thrown.expect(ResourceParseException.class);
    thrown.expectMessage(
        "Invalid resource type 'unknown_type' in <public> resource 'some_resource_name' in file"
            + " 'public.xml'");

    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<public name=\"some_resource_name\" type=\"unknown_type\"/>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(tmpFolder.getRoot(), lines, Paths.get("public.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("public.xml")));
  }

  @Test
  public void validPublicResourceTypeInInvalidFile() throws IOException, ResourceParseException {
    thrown.expect(ResourceParseException.class);
    thrown.expectMessage(
        "<public> resource 'some_resource_name' must be declared in res/values/public.xml, but was"
            + " declared in 'non-public.xml'");

    ImmutableList<String> lines =
        ImmutableList.<String>builder()
            .add(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<resources>",
                "<public name=\"some_resource_name\" type=\"string\"/>",
                "</resources>")
            .build();

    ProjectFilesystemUtils.writeLinesToPath(
        tmpFolder.getRoot(), lines, Paths.get("non-public.xml"));

    MiniAapt aapt = new MiniAapt(ImmutableSet.of());

    aapt.processValuesFile(
        ProjectFilesystemUtils.getPathForRelativePath(
            tmpFolder.getRoot(), Paths.get("non-public.xml")));
  }
}
