/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd;

import java.io.PrintStream;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A PrintStream implementation that intercepts stderr output and applies syntax highlighting to
 * compiler messages based on the file type
 */
public class ErrorInterceptor extends PrintStream {

  private static final String RED = "\033[91m";
  private static final String YELLOW = "\033[93m";
  private static final String GREEN = "\033[92m";
  private static final String MAGENTA = "\033[95m";
  private static final String BLUE = "\033[94m";
  private static final String BOLD = "\033[1m";
  private static final String RESET = "\033[0m";

  // Source: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html
  private static final String[] JAVA_KEYWORDS = {
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extends",
    "final",
    "finally",
    "float",
    "for",
    "goto",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "native",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "strictfp",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "try",
    "void",
    "volatile",
    "while",
    "null",
    "true",
    "false",
    "record",
    "sealed",
    "permits",
    "non-sealed",
    "var"
  };

  // Source: https://kotlinlang.org/docs/keyword-reference.html
  private static final String[] KOTLIN_KEYWORDS = {
    "as",
    "as?",
    "break",
    "class",
    "continue",
    "do",
    "else",
    "false",
    "for",
    "fun",
    "if",
    "in",
    "!in",
    "interface",
    "is",
    "!is",
    "null",
    "object",
    "package",
    "return",
    "super",
    "this",
    "throw",
    "true",
    "try",
    "typealias",
    "val",
    "var",
    "when",
    "while",
    "by",
    "catch",
    "constructor",
    "delegate",
    "dynamic",
    "field",
    "file",
    "finally",
    "get",
    "import",
    "init",
    "param",
    "property",
    "receiver",
    "set",
    "setparam",
    "where",
    "actual",
    "abstract",
    "annotation",
    "companion",
    "const",
    "crossinline",
    "data",
    "enum",
    "expect",
    "external",
    "final",
    "infix",
    "inline",
    "inner",
    "internal",
    "lateinit",
    "noinline",
    "open",
    "operator",
    "out",
    "override",
    "private",
    "protected",
    "public",
    "reified",
    "sealed",
    "suspend",
    "tailrec",
    "vararg",
    "field",
    "it"
  };

  private static final Pattern ERROR_PATTERN =
      Pattern.compile("\\berror\\b(:.*?\\n)((?:.*?\\n)+?\\s*)(\\^+)\\n", Pattern.CASE_INSENSITIVE);
  private static final Pattern WARNING_PATTERN =
      Pattern.compile("\\bwarning\\b(:.*?)\\n", Pattern.CASE_INSENSITIVE);
  private static final Pattern JAVA_FILE_PATTERN =
      Pattern.compile("\\b(/?(?:\\w+/)*\\w+\\.java)\\b:(\\d+):");
  private static final Pattern KOTLIN_FILE_PATTERN =
      Pattern.compile("\\b(/?(?:\\w+/)*\\w+\\.kt)\\b:(\\d+):(\\d+):");

  public ErrorInterceptor() {
    super(System.err);
  }

  @Override
  public void print(String message) {
    super.print(prettyPrint(message));
  }

  public static String prettyPrint(String errorMessage) {
    if (errorMessage == null || errorMessage.isEmpty()) {
      return errorMessage;
    }

    if (System.getenv("NO_COLOR") != null) {
      return errorMessage;
    }

    String fileType = determineFileType(errorMessage);

    if ("java".equals(fileType)) {
      return prettyPrintJavaError(errorMessage);
    } else if ("kotlin".equals(fileType)) {
      return prettyPrintKotlinError(errorMessage);
    } else {
      return errorMessage;
    }
  }

  private static String determineFileType(String errorMessage) {
    Matcher javaMatcher = JAVA_FILE_PATTERN.matcher(errorMessage);
    if (javaMatcher.find()) {
      return "java";
    }

    Matcher kotlinMatcher = KOTLIN_FILE_PATTERN.matcher(errorMessage);
    if (kotlinMatcher.find()) {
      return "kotlin";
    }

    // Default to "unknown" if file type cannot be determined
    return "unknown";
  }

  private static String prettyPrintJavaError(String message) {
    message =
        colorizePattern(ERROR_PATTERN, message, match -> highlightError(match, JAVA_KEYWORDS));
    message = colorizePattern(WARNING_PATTERN, message, ErrorInterceptor::highlightWarning);
    message = colorizePattern(JAVA_FILE_PATTERN, message, ErrorInterceptor::highlightJavaFile);
    return message;
  }

  private static String prettyPrintKotlinError(String message) {
    message =
        colorizePattern(ERROR_PATTERN, message, match -> highlightError(match, KOTLIN_KEYWORDS));
    message = colorizePattern(WARNING_PATTERN, message, ErrorInterceptor::highlightWarning);
    message = colorizePattern(KOTLIN_FILE_PATTERN, message, ErrorInterceptor::highlightKotlinFile);
    return message;
  }

  /**
   * Applies a colorizer function to all matches of a pattern in a string.
   *
   * @param pattern The pattern to match
   * @param message The string to search
   * @param colorizer The function to apply to each match
   * @return The colorized string
   */
  private static String colorizePattern(
      Pattern pattern, String message, Function<Matcher, String> colorizer) {
    Matcher matcher = pattern.matcher(message);
    StringBuffer result = new StringBuffer();
    while (matcher.find()) {
      String replacement = colorizer.apply(matcher);
      matcher.appendReplacement(result, Matcher.quoteReplacement(replacement));
    }
    matcher.appendTail(result);
    return result.toString();
  }

  private static String highlightError(Matcher match, String[] keywords) {
    return RED
        + "error"
        + RESET
        + BOLD
        + match.group(1)
        + RESET
        + highlightCode(match.group(2), keywords)
        + RED
        + match.group(3)
        + RESET
        + "\n";
  }

  private static String highlightWarning(Matcher match) {
    return YELLOW + "warning" + RESET + BOLD + match.group(1) + RESET + "\n";
  }

  private static String highlightJavaFile(Matcher match) {
    return GREEN + match.group(1) + RESET + ":" + MAGENTA + match.group(2) + RESET + ":";
  }

  private static String highlightKotlinFile(Matcher match) {
    return GREEN
        + match.group(1)
        + RESET
        + ":"
        + MAGENTA
        + match.group(2)
        + RESET
        + ":"
        + MAGENTA
        + match.group(3)
        + RESET
        + ":";
  }

  private static String highlightCode(String code, String[] keywords) {
    return code.replaceAll("\\b(" + String.join("|", keywords) + ")\\b", BLUE + "$1" + RESET);
  }
}
