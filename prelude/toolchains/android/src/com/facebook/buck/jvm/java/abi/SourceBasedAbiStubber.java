/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi;

import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.jvm.java.abi.source.api.SourceOnlyAbiRuleInfoFactory.SourceOnlyAbiRuleInfo;
import com.facebook.buck.jvm.java.plugin.api.BuckJavacTaskListener;
import com.facebook.buck.jvm.java.plugin.api.BuckJavacTaskProxy;
import com.facebook.buck.jvm.java.plugin.api.PluginClassLoader;
import java.lang.reflect.Constructor;
import java.util.function.Supplier;
import javax.tools.Diagnostic;

public final class SourceBasedAbiStubber {
  public static BuckJavacTaskListener newValidatingTaskListener(
      PluginClassLoader pluginLoader,
      BuckJavacTaskProxy task,
      SourceOnlyAbiRuleInfo ruleInfo,
      Supplier<Boolean> errorsExist,
      Diagnostic.Kind messageKind) {
    try {
      Class<?> validatingTaskListenerClass =
          pluginLoader.loadClass(
              "com.facebook.buck.jvm.java.abi.source.ValidatingTaskListener", Object.class);
      Constructor<?> constructor =
          validatingTaskListenerClass.getConstructor(
              BuckJavacTaskProxy.class,
              SourceOnlyAbiRuleInfo.class,
              Supplier.class,
              Diagnostic.Kind.class);

      return BuckJavacTaskListener.wrapRealTaskListener(
          pluginLoader, constructor.newInstance(task, ruleInfo, errorsExist, messageKind));
    } catch (ReflectiveOperationException e) {
      throw new HumanReadableException(
          e,
          "Could not load source-generated ABI validator. Your compiler might not support this. "
              + "If it doesn't, you may need to disable source-based ABI generation.");
    }
  }

  private SourceBasedAbiStubber() {}
}
