// @generated
// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: worker.proto

// Protobuf Java Version: 3.25.6
package com.facebook.buck.worker.model;

@javax.annotation.Generated(value="protoc", comments="annotations:ExecuteResponseOrBuilder.java.pb.meta")
public interface ExecuteResponseOrBuilder extends
    // @@protoc_insertion_point(interface_extends:worker.ExecuteResponse)
    com.google.protobuf.MessageOrBuilder {

  /**
   * <code>int32 exit_code = 1;</code>
   * @return The exitCode.
   */
  int getExitCode();

  /**
   * <code>string stderr = 2;</code>
   * @return The stderr.
   */
  java.lang.String getStderr();
  /**
   * <code>string stderr = 2;</code>
   * @return The bytes for stderr.
   */
  com.google.protobuf.ByteString
      getStderrBytes();

  /**
   * <code>uint64 timed_out_after_s = 3;</code>
   * @return The timedOutAfterS.
   */
  long getTimedOutAfterS();
}
