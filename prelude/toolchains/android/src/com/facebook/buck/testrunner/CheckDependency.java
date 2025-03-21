/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testrunner;

import java.lang.annotation.Annotation;
import java.util.Optional;

class CheckDependency {

  private CheckDependency() {
    // Utility class.
  }

  /**
   * Validate the presence of the class or execute {@code System.exit(1)} if not.
   *
   * @param classToLoad name of the class that is required for the execution to continue.
   */
  public static void requiresClass(String name, String classToLoad) {
    try {
      Class.forName(classToLoad);
    } catch (ClassNotFoundException e) {
      System.err.println(
          "Unable to locate " + name + " on the classpath. Please add as a test dependency.");
      System.exit(1);
    }
  }

  /**
   * @param classToLoad name of the class that should be loaded.
   * @return Optional containing a reference to the class if it exists, otherwise {@code
   *     Optional.empty()}
   */
  public static Optional<Class<?>> optionalClass(String classToLoad) {
    try {
      return Optional.ofNullable(Class.forName(classToLoad));
    } catch (ClassNotFoundException e) {
      return Optional.empty();
    }
  }

  /**
   * @param annotationName name of the optional annotation.
   * @return Optional containing a reference to the annotation class if it exists, otherwise {@code
   *     Optional.empty()}
   */
  @SuppressWarnings("unchecked")
  public static Optional<Class<? extends Annotation>> optionalAnnotation(String annotationName) {
    return optionalClass(annotationName)
        .filter(Class::isAnnotation)
        .map(ann -> (Class<? extends Annotation>) ann);
  }

  /**
   * @param className name of the class that should have its presence checked.
   * @return true if the class is present in the classpath.
   */
  public static boolean classPresent(String className) {
    return optionalClass(className).isPresent();
  }
}
