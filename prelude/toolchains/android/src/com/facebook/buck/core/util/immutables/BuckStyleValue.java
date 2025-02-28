/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.util.immutables;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.immutables.value.Value;

/**
 * Value-style objects.
 *
 * <p>The generated immutable will be package private only so that the immutable generated class is
 * not used. The declaring interface/abstract class should re-expose constructors as appropriate
 * (like auto-value). It is intended that all users of the immutable ever only refer to the
 * interface/abstract class so that the underlying immmutable implementation can change freely.
 *
 * <p>Value-style objects have all attributes as "of" constructor parameters, do not have builders,
 * "with" methods, "copy" methods.
 *
 * <p>When using this annotation, it's unnecessary to explicitly use @Value.Immutable.
 *
 * <p>If the Value.Immutable behavior needs to be customized, you must explicitly set the values set
 * in the defaults here (otherwise a linter will yell at you).
 *
 * @see <a href="http://immutables.github.io/immutable.html#tuples">Immutable user guide</a>
 */
@Value.Style(
    get = {"is*", "get*"},
    init = "set*",
    of = "ofImpl",
    visibility = Value.Style.ImplementationVisibility.PACKAGE,
    allParameters = true,
    defaults = @Value.Immutable(builder = false, copy = false, prehash = false),
    forceJacksonPropertyNames = false,
    additionalJsonAnnotations = {JsonNaming.class})
@Target({ElementType.TYPE, ElementType.PACKAGE, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface BuckStyleValue {}
