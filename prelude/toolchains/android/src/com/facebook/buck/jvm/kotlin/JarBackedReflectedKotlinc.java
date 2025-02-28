/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import static com.google.common.collect.Iterables.transform;

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.javax.SynchronizedToolProvider;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext;
import com.facebook.buck.jvm.kotlin.kotlinc.Kotlinc;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.facebook.buck.util.ClassLoaderCache;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.nio.file.Path;
import java.util.Optional;

public class JarBackedReflectedKotlinc implements Kotlinc {

  private static final Logger LOG = Logger.get(JarBackedReflectedKotlinc.class);
  private static final String COMPILER_CLASS = "org.jetbrains.kotlin.cli.jvm.K2JVMCompiler";
  private static final String EXIT_CODE_CLASS = "org.jetbrains.kotlin.cli.common.ExitCode";

  @Override
  public String getDescription(
      ImmutableList<String> options,
      ImmutableSortedSet<RelPath> javaSourceFilePaths,
      Path pathToSrcsList) {
    StringBuilder builder = new StringBuilder("kotlinc ");
    Joiner.on(" ").appendTo(builder, options);
    builder.append(" ");
    builder.append("@").append(pathToSrcsList);

    return builder.toString();
  }

  @Override
  public String getShortName() {
    return "kotlinc";
  }

  @Override
  public int buildWithClasspath(
      IsolatedExecutionContext context,
      BuildTargetValue invokingRule,
      ImmutableList<String> options,
      ImmutableList<AbsPath> kotlinHomeLibraries,
      ImmutableSortedSet<RelPath> kotlinSourceFilePaths,
      Path pathToSrcsList,
      Optional<Path> workingDirectory,
      AbsPath ruleCellRoot,
      KotlincMode mode,
      KotlinCDLoggingContext loggingContext) {

    ImmutableList<Path> expandedSources;
    try {
      expandedSources =
          getExpandedSourcePaths(ruleCellRoot, kotlinSourceFilePaths, workingDirectory);
    } catch (Throwable throwable) {
      throwable.printStackTrace();
      throw new HumanReadableException(
          "Unable to expand sources for %s into %s",
          invokingRule.getFullyQualifiedName(), workingDirectory);
    }

    ImmutableList<String> args =
        ImmutableList.<String>builder()
            .addAll(options)
            .addAll(transform(expandedSources, path -> ruleCellRoot.resolve(path).toString()))
            .build();

    try {
      Object compilerShim = loadCompilerShim(context, kotlinHomeLibraries);

      Method compile = compilerShim.getClass().getMethod("exec", PrintStream.class, String[].class);

      Class<?> exitCodeClass = compilerShim.getClass().getClassLoader().loadClass(EXIT_CODE_CLASS);

      Method getCode = exitCodeClass.getMethod("getCode");

      try (UncloseablePrintStream stdErr = new UncloseablePrintStream(context.getStdErr())) {
        LOG.info(
            "[KotlinC Build Step for target:%s type:%s] Running %s with arguments:[%s]",
            invokingRule.getFullyQualifiedName(),
            invokingRule.getType().toString(),
            COMPILER_CLASS,
            String.join(", ", args));
        context.getStdOut().println("KotlinC: " + this.getClass().getSimpleName());
        Object exitCode = compile.invoke(compilerShim, stdErr, args.toArray(new String[0]));

        return (Integer) getCode.invoke(exitCode);
      }

    } catch (IllegalAccessException
        | InvocationTargetException
        | IOException
        | NoSuchMethodException
        | ClassNotFoundException ex) {
      throw new RuntimeException(ex);
    }
  }

  private Object loadCompilerShim(
      IsolatedExecutionContext context, ImmutableList<AbsPath> kotlinHomeLibraries)
      throws IOException {
    try (ClassLoaderCache classLoaderCache = context.getClassLoaderCache()) {
      classLoaderCache.addRef();
      ClassLoader classLoader =
          classLoaderCache.getClassLoaderForClassPath(
              SynchronizedToolProvider.getSystemToolClassLoader(),
              ImmutableList.copyOf(
                  kotlinHomeLibraries.stream()
                      .map(
                          absPath -> {
                            try {
                              return absPath.getPath().toUri().toURL();
                            } catch (MalformedURLException e) {
                              throw new RuntimeException(e);
                            }
                          })
                      .iterator()));

      return classLoader.loadClass(COMPILER_CLASS).newInstance();
    } catch (Exception ex) {
      throw new RuntimeException(ex);
    }
  }

  private static class UncloseablePrintStream extends PrintStream {

    UncloseablePrintStream(PrintStream delegate) {
      super(delegate);
    }

    @Override
    public void close() {
      // ignore
    }
  }
}
