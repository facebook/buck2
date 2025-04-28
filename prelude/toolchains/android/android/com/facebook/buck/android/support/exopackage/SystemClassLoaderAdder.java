/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.support.exopackage;

import android.annotation.TargetApi;
import android.os.Build;
import dalvik.system.BaseDexClassLoader;
import dalvik.system.DexClassLoader;
import dalvik.system.PathClassLoader;
import java.io.File;
import java.lang.reflect.Array;
import java.util.List;

/**
 * Uses reflection to modify the system class loader. There's no way to override or replace the
 * system class loader with our own class loader that knows how to load our pre-dexed jars. This
 * uses reflection to modify the system class loader. This was written based on careful inspection
 * of the source Android source for {@link DexClassLoader} and {@link PathClassLoader}.
 */
public class SystemClassLoaderAdder {

  private SystemClassLoaderAdder() {}

  /**
   * Installs a list of .dex.jar files into the application class loader.
   *
   * @param appClassLoader The application ClassLoader, which can be retrieved by calling {@code
   *     getClassLoader} on the application Context.
   * @param optimizedDirectory Directory for storing optimized dex files.
   * @param dexJars The list of .dex.jar files to load.
   */
  public static void installDexJars(
      ClassLoader appClassLoader, File optimizedDirectory, List<File> dexJars) {
    SystemClassLoaderAdder classLoaderAdder = new SystemClassLoaderAdder();

    for (File dexJar : dexJars) {
      DexClassLoader newClassLoader =
          new DexClassLoader(
              dexJar.getAbsolutePath(), optimizedDirectory.getAbsolutePath(), null, appClassLoader);
      classLoaderAdder.addPathsOfClassLoaderToSystemClassLoader(
          newClassLoader, (PathClassLoader) appClassLoader);
    }
  }

  /**
   * Adds the paths in {@code newClassLoader} to the paths in {@code systemClassLoader} using
   * reflection since there's no way to do this with public APIs.
   *
   * @param newClassLoader the class loader with the new paths
   * @param systemClassLoader the system class loader
   */
  private void addPathsOfClassLoaderToSystemClassLoader(
      DexClassLoader newClassLoader, PathClassLoader systemClassLoader) {

    try {
      if (existsBaseDexClassLoader()) {
        // In ICS, PathClassLoader and DexClassLoader are built on a common base class called
        // BaseDexClassLoader.
        addNewClassLoaderToSystemClassLoaderWithBaseDex(newClassLoader, systemClassLoader);
      } else {
        // In earlier versions, PathClassLoader and DexClassLoader have similar layout but
        // don't share code.
        addNewClassLoaderToSystemClassLoaderPreBaseDex(newClassLoader, systemClassLoader);
      }
    } catch (NoSuchFieldException e) {
      throw new RuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new RuntimeException(e);
    }
  }

  private boolean existsBaseDexClassLoader() {
    try {
      Class.forName("dalvik.system.BaseDexClassLoader");
      return true;
    } catch (ClassNotFoundException e) {
      return false;
    }
  }

  /**
   * Adds the paths in {@code newClassLoader} to the paths in {@code systemClassLoader}. This works
   * with versions of Android that have {@link BaseDexClassLoader}.
   *
   * @param newClassLoader the class loader with the new paths
   * @param systemClassLoader the system class loader
   */
  private void addNewClassLoaderToSystemClassLoaderWithBaseDex(
      DexClassLoader newClassLoader, PathClassLoader systemClassLoader)
      throws NoSuchFieldException, IllegalAccessException {
    Object currentElementsArray = getDexElementsArray(getDexPathList(systemClassLoader));
    Object newElementsArray = getDexElementsArray(getDexPathList(newClassLoader));
    Object mergedElementsArray = mergeArrays(currentElementsArray, newElementsArray);
    setDexElementsArray(getDexPathList(systemClassLoader), mergedElementsArray);
  }

  /**
   * Adds the paths in {@code newClassLoader} to the paths in {@code systemClassLoader}. This works
   * with versions of Android that pre-date {@link BaseDexClassLoader}.
   *
   * @param newClassLoader the class loader with the new paths
   * @param systemClassLoader the system class loader
   */
  @SuppressWarnings("PMD.EmptyCatchBlock")
  private void addNewClassLoaderToSystemClassLoaderPreBaseDex(
      DexClassLoader newClassLoader, PathClassLoader systemClassLoader)
      throws NoSuchFieldException, IllegalAccessException {

    try {
      // The class loader is lazily initialized. Loading any class forces it to be initialized.
      newClassLoader.loadClass("foo");
    } catch (ClassNotFoundException e) {
      // Expected to fail.
    }

    // Copy value in scalar field newClassLoader.mRawDexPath and array field
    // systemClassLoader.mPaths to a new array that replaces systemClassLoader.mPaths.
    Reflect.setField(
        systemClassLoader,
        PathClassLoader.class,
        "mPaths",
        mergeArrayAndScalar(
            Reflect.getField(systemClassLoader, PathClassLoader.class, "mPaths"),
            Reflect.getField(newClassLoader, DexClassLoader.class, "mRawDexPath")));

    // Copy values in array field newClassLoader.mFiles and array field systemClassLoader.mFiles
    // to a new array that replaces systemClassLoader.mFiles.
    Reflect.setField(
        systemClassLoader,
        PathClassLoader.class,
        "mFiles",
        mergeArrays(
            Reflect.getField(systemClassLoader, PathClassLoader.class, "mFiles"),
            Reflect.getField(newClassLoader, DexClassLoader.class, "mFiles")));

    // Copy values in array field newClassLoader.mZips and array field systemClassLoader.mZips
    // to a new array that replaces systemClassLoader.mZips.
    Reflect.setField(
        systemClassLoader,
        PathClassLoader.class,
        "mZips",
        mergeArrays(
            Reflect.getField(systemClassLoader, PathClassLoader.class, "mZips"),
            Reflect.getField(newClassLoader, DexClassLoader.class, "mZips")));

    // Copy values in array field newClassLoader.mDexs and array field systemClassLoader.mDexs
    // to a new array that replaces systemClassLoader.mDexs.
    Reflect.setField(
        systemClassLoader,
        PathClassLoader.class,
        "mDexs",
        mergeArrays(
            Reflect.getField(systemClassLoader, PathClassLoader.class, "mDexs"),
            Reflect.getField(newClassLoader, DexClassLoader.class, "mDexs")));
  }

  @TargetApi(Build.VERSION_CODES.ICE_CREAM_SANDWICH)
  private Object getDexPathList(BaseDexClassLoader classLoader)
      throws NoSuchFieldException, IllegalAccessException {
    return Reflect.getField(classLoader, BaseDexClassLoader.class, "pathList");
  }

  private Object getDexElementsArray(Object dexPathList)
      throws NoSuchFieldException, IllegalAccessException {
    return Reflect.getField(dexPathList, dexPathList.getClass(), "dexElements");
  }

  private void setDexElementsArray(Object dexPathList, Object newElementArray)
      throws NoSuchFieldException, IllegalAccessException {
    Reflect.setField(dexPathList, dexPathList.getClass(), "dexElements", newElementArray);
  }

  private Object mergeArrays(Object array1, Object array2) {
    Class<?> arrayClass = array1.getClass();
    Class<?> itemClass = arrayClass.getComponentType();
    int array1Size = Array.getLength(array1);
    int array2Size = Array.getLength(array2);
    int newSize = array1Size + array2Size;
    Object newArray = Array.newInstance(itemClass, newSize);
    for (int i = 0; i < newSize; i++) {
      if (i < array1Size) {
        Array.set(newArray, i, Array.get(array1, i));
      } else {
        Array.set(newArray, i, Array.get(array2, i - array1Size));
      }
    }
    return newArray;
  }

  private Object mergeArrayAndScalar(Object array, Object scalar) {
    Class<?> arrayClass = array.getClass();
    Class<?> itemClass = arrayClass.getComponentType();
    int array1Size = Array.getLength(array);
    int newSize = array1Size + 1;
    Object newArray = Array.newInstance(itemClass, newSize);
    for (int i = 0; i < newSize; i++) {
      if (i < array1Size) {
        Array.set(newArray, i, Array.get(array, i));
      } else {
        Array.set(newArray, i, scalar);
      }
    }
    return newArray;
  }
}
