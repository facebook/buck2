/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.cd.workertool;

import java.io.PrintStream;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A PrintStream implementation that intercepts stderr output and applies syntax highlighting to
 * Kotlin compiler messages.
 */
public class KotlinStdErrInterceptor extends PrintStream {
  public KotlinStdErrInterceptor() {
    super(System.err);
  }

  static String RED = "\033[91m";
  static String YELLOW = "\033[93m";
  static String GREEN = "\033[92m";
  static String MAGENTA = "\033[95m";
  static String BLUE = "\033[94m";
  static String BOLD = "\033[1m";
  static String RESET = "\033[0m";

  // Source: https://kotlinlang.org/docs/keyword-reference.html
  static String[] kotlinKeywords = {
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
    "it",
  };

  private static Pattern errorPattern =
      Pattern.compile("\\berror\\b(:.*?\\n)((?:.*?\\n)+?\\s*)(\\^+)\\n", Pattern.CASE_INSENSITIVE);

  private static Pattern warningPattern =
      Pattern.compile("\\bwarning\\b(:.*?)\\n", java.util.regex.Pattern.CASE_INSENSITIVE);

  private static Pattern filePattern =
      Pattern.compile("\\b(/?(?:\\w+/)*\\w+\\.\\w{2,})\\b:(\\d+):(\\d+):");

  @Override
  public void print(String message) {
    super.print(prettyPrint(message));
  }

  public static String prettyPrint(String message) {

    if (System.getenv("NO_COLOR") != null || System.getenv("NO_KOTLINC_COLOR") != null) {
      return message;
    }

    message = colorizePattern(errorPattern, message, KotlinStdErrInterceptor::highlightKotlinError);
    message =
        colorizePattern(warningPattern, message, KotlinStdErrInterceptor::highlightKotlinWarning);
    message = colorizePattern(filePattern, message, KotlinStdErrInterceptor::highlightKotlinFile);

    return message;
  }

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

  private static String highlightKotlinError(Matcher match) {
    return RED
        + "error"
        + RESET
        + BOLD
        + match.group(1)
        + RESET
        + highlightKotlinCode(match.group(2))
        + RED
        + match.group(3)
        + RESET
        + "\n";
  }

  private static String highlightKotlinWarning(Matcher match) {
    return YELLOW + "warning" + RESET + BOLD + match.group(1) + RESET + "\n";
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

  private static String highlightKotlinCode(String s) {
    return s.replaceAll("\\b(" + String.join("|", kotlinKeywords) + ")\\b", BLUE + "$1" + RESET);
  }
}
