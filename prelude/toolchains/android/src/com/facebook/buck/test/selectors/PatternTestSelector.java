/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.test.selectors;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * A {@link TestDescription} will match if this selector's class-part is a substring of the {@link
 * TestDescription}'s full class-name, or if this selector's class-name, when interpreted as a
 * java.util.regex regular-expression, matches the {@link TestDescription}'s full class-name.
 *
 * <p>(The same rules apply for the method-name as well. If this selector's class-part or
 * method-part are null, all class-names or method-names will match.)
 */
public class PatternTestSelector implements TestSelector {

  static final PatternTestSelector INCLUDE_EVERYTHING = new PatternTestSelector(true, null, null);
  static final PatternTestSelector EXCLUDE_EVERYTHING = new PatternTestSelector(false, null, null);

  private final boolean inclusive;
  @Nullable private final Pattern classPattern;
  @Nullable private final Pattern methodPattern;

  private PatternTestSelector(
      boolean inclusive, @Nullable Pattern classPattern, @Nullable Pattern methodPattern) {
    this.inclusive = inclusive;
    this.classPattern = classPattern;
    this.methodPattern = methodPattern;
  }

  /**
   * Build a {@link PatternTestSelector} from the given String. Selector strings should be of the
   * form "[is-exclusive][class-part]#[method-part]". If "[is-exclusive]" is a "!" then this
   * selector will exclude tests, otherwise it will include tests.
   *
   * <p>If the class-part (or method-part) are omitted, then all classes or methods will match.
   * Consequently "#" means "include everything" and "!#" means "exclude everything".
   *
   * <p>If the selector string doesn't contain a "#" at all, it is interpreted as a class-part.
   *
   * @param rawSelectorString An unparsed selector string.
   */
  public static TestSelector buildFromSelectorString(String rawSelectorString) {
    if (rawSelectorString == null || rawSelectorString.isEmpty()) {
      throw new RuntimeException("Cannot build from a null or empty string!");
    }

    boolean isInclusive = true;
    String remainder;
    if (rawSelectorString.charAt(0) == '!') {
      isInclusive = false;
      remainder = rawSelectorString.substring(1);
    } else {
      remainder = rawSelectorString;
    }
    // Reuse universal inclusion
    if (remainder.equals("#")) {
      return isInclusive ? INCLUDE_EVERYTHING : EXCLUDE_EVERYTHING;
    }

    Pattern classPattern;
    Pattern methodPattern = null;
    // we only apply splitting once. parameterized tests can have parameterization names that
    // contains the symbol "#" in its method. e.g ClassTest#methodTest[Param#name]. So we should
    // only split on the first "#" as class names cannot contain that symbol.
    String[] parts = remainder.split("#", 2);

    try {
      if (parts.length == 1) {
        // "com.example.Test", "com.example.Test#"
        classPattern = getPatternOrNull(parts[0]);
      } else {
        if (parts[1].startsWith("#")) {
          throw new TestSelectorParseException(
              String.format("Invalid method pattern ## in %s", rawSelectorString));
        }
        // "com.example.Test#testX", "#testX", "#"
        classPattern = getPatternOrNull(parts[0]);
        methodPattern = getPatternOrNull(parts[1]);
      }
    } catch (PatternSyntaxException e) {
      throw new TestSelectorParseException(
          String.format("Regular expression error in '%s': %s", rawSelectorString, e.getMessage()));
    }

    return new PatternTestSelector(isInclusive, classPattern, methodPattern);
  }

  @Override
  public String getRawSelector() {
    StringBuilder builder = new StringBuilder();
    if (!inclusive) {
      builder.append('!');
    }
    if (classPattern != null) {
      builder.append(classPattern);
    }
    builder.append('#');
    if (methodPattern != null) {
      builder.append(methodPattern);
    }
    return builder.toString();
  }

  @Nullable
  private static Pattern getPatternOrNull(String string) {
    if (string.isEmpty()) {
      return null;
    } else {
      if (!string.endsWith("$")) {
        string = string + "$";
      }
      return Pattern.compile(string);
    }
  }

  @Override
  public String getExplanation() {
    if (isMatchAnyClass() && isMatchAnyMethod()) {
      return isInclusive() ? "include everything else" : "exclude everything else";
    }
    return String.format(
        "%s class:%s method:%s",
        isInclusive() ? "include" : "exclude",
        isMatchAnyClass() ? "<any>" : classPattern,
        isMatchAnyMethod() ? "<any>" : methodPattern);
  }

  @Override
  public boolean isInclusive() {
    return inclusive;
  }

  @Override
  public boolean isMatchAnyClass() {
    return classPattern == null;
  }

  @Override
  public boolean isMatchAnyMethod() {
    return methodPattern == null;
  }

  @Override
  public boolean matches(TestDescription description) {
    return matchesClassName(description.getClassName())
        && matchesMethodName(description.getMethodName());
  }

  @Override
  public boolean matchesClassName(String className) {
    if (classPattern == null || className == null) {
      return true;
    }
    return classPattern.matcher(className).find();
  }

  @Override
  public boolean containsClassPath(String classPath) {
    if (classPattern == null) {
      return true;
    }
    // Find occurrence of "\$" in the pattern, which indicates a nested class
    String[] outerInnerPatterns = classPattern.toString().split("\\\\\\$");
    if (outerInnerPatterns.length == 2) {
      // If we have an outer class, we should ensure that we are treating the outer class pattern as
      // matching the end of the classpath, so add a "$"
      // e.g: pattern of "Foo\$Bar" means it should only match outer classpaths of "Foo", not "FooF"
      outerInnerPatterns[0] += "$";
    }
    return Pattern.compile(outerInnerPatterns[0]).matcher(classPath).find();
  }

  private boolean matchesMethodName(String methodName) {
    if (methodPattern == null || methodName == null) {
      return true;
    }
    return methodPattern.matcher(methodName).find();
  }
}
