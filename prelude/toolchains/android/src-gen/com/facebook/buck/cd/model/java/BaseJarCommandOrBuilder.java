// @generated
// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: javacd.proto

package com.facebook.buck.cd.model.java;

@javax.annotation.Generated(value="protoc", comments="annotations:BaseJarCommandOrBuilder.java.pb.meta")
public interface BaseJarCommandOrBuilder extends
    // @@protoc_insertion_point(interface_extends:javacd.api.v1.BaseJarCommand)
    com.google.protobuf.MessageOrBuilder {

  /**
   * <code>.javacd.api.v1.AbiGenerationMode abiCompatibilityMode = 1;</code>
   */
  int getAbiCompatibilityModeValue();
  /**
   * <code>.javacd.api.v1.AbiGenerationMode abiCompatibilityMode = 1;</code>
   */
  com.facebook.buck.cd.model.java.AbiGenerationMode getAbiCompatibilityMode();

  /**
   * <code>.javacd.api.v1.AbiGenerationMode abiGenerationMode = 2;</code>
   */
  int getAbiGenerationModeValue();
  /**
   * <code>.javacd.api.v1.AbiGenerationMode abiGenerationMode = 2;</code>
   */
  com.facebook.buck.cd.model.java.AbiGenerationMode getAbiGenerationMode();

  /**
   * <code>bool isRequiredForSourceOnlyAbi = 3;</code>
   */
  boolean getIsRequiredForSourceOnlyAbi();

  /**
   * <code>bool trackClassUsage = 4;</code>
   */
  boolean getTrackClassUsage();

  /**
   * <code>string configuredBuckOut = 6;</code>
   */
  java.lang.String getConfiguredBuckOut();
  /**
   * <code>string configuredBuckOut = 6;</code>
   */
  com.google.protobuf.ByteString
      getConfiguredBuckOutBytes();

  /**
   * <code>.javacd.api.v1.BuildTargetValue buildTargetValue = 7;</code>
   */
  boolean hasBuildTargetValue();
  /**
   * <code>.javacd.api.v1.BuildTargetValue buildTargetValue = 7;</code>
   */
  com.facebook.buck.cd.model.java.BuildTargetValue getBuildTargetValue();
  /**
   * <code>.javacd.api.v1.BuildTargetValue buildTargetValue = 7;</code>
   */
  com.facebook.buck.cd.model.java.BuildTargetValueOrBuilder getBuildTargetValueOrBuilder();

  /**
   * <code>.javacd.api.v1.OutputPathsValue outputPathsValue = 8;</code>
   */
  boolean hasOutputPathsValue();
  /**
   * <code>.javacd.api.v1.OutputPathsValue outputPathsValue = 8;</code>
   */
  com.facebook.buck.cd.model.java.OutputPathsValue getOutputPathsValue();
  /**
   * <code>.javacd.api.v1.OutputPathsValue outputPathsValue = 8;</code>
   */
  com.facebook.buck.cd.model.java.OutputPathsValueOrBuilder getOutputPathsValueOrBuilder();

  /**
   * <code>repeated string compileTimeClasspathPaths = 9;</code>
   */
  java.util.List<java.lang.String>
      getCompileTimeClasspathPathsList();
  /**
   * <code>repeated string compileTimeClasspathPaths = 9;</code>
   */
  int getCompileTimeClasspathPathsCount();
  /**
   * <code>repeated string compileTimeClasspathPaths = 9;</code>
   */
  java.lang.String getCompileTimeClasspathPaths(int index);
  /**
   * <code>repeated string compileTimeClasspathPaths = 9;</code>
   */
  com.google.protobuf.ByteString
      getCompileTimeClasspathPathsBytes(int index);

  /**
   * <code>repeated string javaSrcs = 10;</code>
   */
  java.util.List<java.lang.String>
      getJavaSrcsList();
  /**
   * <code>repeated string javaSrcs = 10;</code>
   */
  int getJavaSrcsCount();
  /**
   * <code>repeated string javaSrcs = 10;</code>
   */
  java.lang.String getJavaSrcs(int index);
  /**
   * <code>repeated string javaSrcs = 10;</code>
   */
  com.google.protobuf.ByteString
      getJavaSrcsBytes(int index);

  /**
   * <code>repeated .RelPathMapEntry resourcesMap = 13;</code>
   */
  java.util.List<com.facebook.buck.cd.model.common.RelPathMapEntry> 
      getResourcesMapList();
  /**
   * <code>repeated .RelPathMapEntry resourcesMap = 13;</code>
   */
  com.facebook.buck.cd.model.common.RelPathMapEntry getResourcesMap(int index);
  /**
   * <code>repeated .RelPathMapEntry resourcesMap = 13;</code>
   */
  int getResourcesMapCount();
  /**
   * <code>repeated .RelPathMapEntry resourcesMap = 13;</code>
   */
  java.util.List<? extends com.facebook.buck.cd.model.common.RelPathMapEntryOrBuilder> 
      getResourcesMapOrBuilderList();
  /**
   * <code>repeated .RelPathMapEntry resourcesMap = 13;</code>
   */
  com.facebook.buck.cd.model.common.RelPathMapEntryOrBuilder getResourcesMapOrBuilder(
      int index);

  /**
   * <code>.javacd.api.v1.JarParameters jarParameters = 15;</code>
   */
  boolean hasJarParameters();
  /**
   * <code>.javacd.api.v1.JarParameters jarParameters = 15;</code>
   */
  com.facebook.buck.cd.model.java.JarParameters getJarParameters();
  /**
   * <code>.javacd.api.v1.JarParameters jarParameters = 15;</code>
   */
  com.facebook.buck.cd.model.java.JarParametersOrBuilder getJarParametersOrBuilder();

  /**
   * <code>string buildCellRootPath = 16;</code>
   */
  java.lang.String getBuildCellRootPath();
  /**
   * <code>string buildCellRootPath = 16;</code>
   */
  com.google.protobuf.ByteString
      getBuildCellRootPathBytes();

  /**
   * <code>.javacd.api.v1.ResolvedJavac resolvedJavac = 17;</code>
   */
  boolean hasResolvedJavac();
  /**
   * <code>.javacd.api.v1.ResolvedJavac resolvedJavac = 17;</code>
   */
  com.facebook.buck.cd.model.java.ResolvedJavac getResolvedJavac();
  /**
   * <code>.javacd.api.v1.ResolvedJavac resolvedJavac = 17;</code>
   */
  com.facebook.buck.cd.model.java.ResolvedJavacOrBuilder getResolvedJavacOrBuilder();

  /**
   * <code>.javacd.api.v1.ResolvedJavacOptions resolvedJavacOptions = 18;</code>
   */
  boolean hasResolvedJavacOptions();
  /**
   * <code>.javacd.api.v1.ResolvedJavacOptions resolvedJavacOptions = 18;</code>
   */
  com.facebook.buck.cd.model.java.ResolvedJavacOptions getResolvedJavacOptions();
  /**
   * <code>.javacd.api.v1.ResolvedJavacOptions resolvedJavacOptions = 18;</code>
   */
  com.facebook.buck.cd.model.java.ResolvedJavacOptionsOrBuilder getResolvedJavacOptionsOrBuilder();

  /**
   * <code>map&lt;string, string&gt; compileTimeClasspathSnapshotPaths = 19;</code>
   */
  int getCompileTimeClasspathSnapshotPathsCount();
  /**
   * <code>map&lt;string, string&gt; compileTimeClasspathSnapshotPaths = 19;</code>
   */
  boolean containsCompileTimeClasspathSnapshotPaths(
      java.lang.String key);
  /**
   * Use {@link #getCompileTimeClasspathSnapshotPathsMap()} instead.
   */
  @java.lang.Deprecated
  java.util.Map<java.lang.String, java.lang.String>
  getCompileTimeClasspathSnapshotPaths();
  /**
   * <code>map&lt;string, string&gt; compileTimeClasspathSnapshotPaths = 19;</code>
   */
  java.util.Map<java.lang.String, java.lang.String>
  getCompileTimeClasspathSnapshotPathsMap();
  /**
   * <code>map&lt;string, string&gt; compileTimeClasspathSnapshotPaths = 19;</code>
   */

  java.lang.String getCompileTimeClasspathSnapshotPathsOrDefault(
      java.lang.String key,
      java.lang.String defaultValue);
  /**
   * <code>map&lt;string, string&gt; compileTimeClasspathSnapshotPaths = 19;</code>
   */

  java.lang.String getCompileTimeClasspathSnapshotPathsOrThrow(
      java.lang.String key);

  /**
   * <code>string pathToClasses = 20;</code>
   */
  java.lang.String getPathToClasses();
  /**
   * <code>string pathToClasses = 20;</code>
   */
  com.google.protobuf.ByteString
      getPathToClassesBytes();

  /**
   * <code>string rootOutput = 21;</code>
   */
  java.lang.String getRootOutput();
  /**
   * <code>string rootOutput = 21;</code>
   */
  com.google.protobuf.ByteString
      getRootOutputBytes();

  /**
   * <code>string annotationsPath = 23;</code>
   */
  java.lang.String getAnnotationsPath();
  /**
   * <code>string annotationsPath = 23;</code>
   */
  com.google.protobuf.ByteString
      getAnnotationsPathBytes();
}
