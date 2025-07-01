/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.json;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class ObjectMappersTest {

  @Parameters
  public static Collection<Object[]> getSerializeAndDeserializePathData() {
    List<String> paths = ImmutableList.of("", "/", "a", "a/b", "/a", "some/path", "/some/path");
    List<Boolean> typed = ImmutableList.of(true, false);

    return Lists.cartesianProduct(
            paths.stream().map(Paths::get).collect(Collectors.toList()), typed)
        .stream()
        .map(List::toArray)
        .collect(Collectors.toList());
  }

  static class Obj {
    public Path path;
  }

  private final Path path;
  private final boolean typed;

  public ObjectMappersTest(Path path, boolean typed) {
    this.path = path;
    this.typed = typed;
  }

  @Test
  public void canSerializeAndDeserializePath() throws Exception {
    Obj obj = new Obj();
    obj.path = path;

    ObjectWriter writer = typed ? ObjectMappers.WRITER_WITH_TYPE : ObjectMappers.WRITER;
    ObjectReader reader = typed ? ObjectMappers.READER_WITH_TYPE : ObjectMappers.READER;

    String data = writer.writeValueAsString(obj);

    Obj actual = reader.forType(Obj.class).readValue(data);

    assertEquals(path, actual.path);
  }

  @Test
  public void canDeserializePathKeys() throws Exception {
    String data = "{\"/a/b\":123}";
    Map<Path, Integer> map =
        ObjectMappers.READER.forType(new TypeReference<Map<Path, Integer>>() {}).readValue(data);
    assertEquals(map.size(), 1);
    assertEquals((int) map.get(Paths.get("/a/b")), 123);
  }
}
