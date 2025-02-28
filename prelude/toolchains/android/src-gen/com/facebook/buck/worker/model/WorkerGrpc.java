// @generated
package com.facebook.buck.worker.model;

import static io.grpc.MethodDescriptor.generateFullMethodName;
import static io.grpc.stub.ClientCalls.asyncBidiStreamingCall;
import static io.grpc.stub.ClientCalls.asyncClientStreamingCall;
import static io.grpc.stub.ClientCalls.asyncServerStreamingCall;
import static io.grpc.stub.ClientCalls.asyncUnaryCall;
import static io.grpc.stub.ClientCalls.blockingServerStreamingCall;
import static io.grpc.stub.ClientCalls.blockingUnaryCall;
import static io.grpc.stub.ClientCalls.futureUnaryCall;
import static io.grpc.stub.ServerCalls.asyncBidiStreamingCall;
import static io.grpc.stub.ServerCalls.asyncClientStreamingCall;
import static io.grpc.stub.ServerCalls.asyncServerStreamingCall;
import static io.grpc.stub.ServerCalls.asyncUnaryCall;
import static io.grpc.stub.ServerCalls.asyncUnimplementedStreamingCall;
import static io.grpc.stub.ServerCalls.asyncUnimplementedUnaryCall;

/**
 */
@javax.annotation.Generated(
    value = "by gRPC proto compiler (version 1.10.1)",
    comments = "Source: worker.proto")
public final class WorkerGrpc {

  private WorkerGrpc() {}

  public static final String SERVICE_NAME = "worker.Worker";

  // Static method descriptors that strictly reflect the proto.
  @io.grpc.ExperimentalApi("https://github.com/grpc/grpc-java/issues/1901")
  @java.lang.Deprecated // Use {@link #getExecuteMethod()} instead. 
  public static final io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteCommand,
      com.facebook.buck.worker.model.ExecuteResponse> METHOD_EXECUTE = getExecuteMethodHelper();

  private static volatile io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteCommand,
      com.facebook.buck.worker.model.ExecuteResponse> getExecuteMethod;

  @io.grpc.ExperimentalApi("https://github.com/grpc/grpc-java/issues/1901")
  public static io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteCommand,
      com.facebook.buck.worker.model.ExecuteResponse> getExecuteMethod() {
    return getExecuteMethodHelper();
  }

  private static io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteCommand,
      com.facebook.buck.worker.model.ExecuteResponse> getExecuteMethodHelper() {
    io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteCommand, com.facebook.buck.worker.model.ExecuteResponse> getExecuteMethod;
    if ((getExecuteMethod = WorkerGrpc.getExecuteMethod) == null) {
      synchronized (WorkerGrpc.class) {
        if ((getExecuteMethod = WorkerGrpc.getExecuteMethod) == null) {
          WorkerGrpc.getExecuteMethod = getExecuteMethod = 
              io.grpc.MethodDescriptor.<com.facebook.buck.worker.model.ExecuteCommand, com.facebook.buck.worker.model.ExecuteResponse>newBuilder()
              .setType(io.grpc.MethodDescriptor.MethodType.UNARY)
              .setFullMethodName(generateFullMethodName(
                  "worker.Worker", "Execute"))
              .setSampledToLocalTracing(true)
              .setRequestMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  com.facebook.buck.worker.model.ExecuteCommand.getDefaultInstance()))
              .setResponseMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  com.facebook.buck.worker.model.ExecuteResponse.getDefaultInstance()))
                  .setSchemaDescriptor(new WorkerMethodDescriptorSupplier("Execute"))
                  .build();
          }
        }
     }
     return getExecuteMethod;
  }
  @io.grpc.ExperimentalApi("https://github.com/grpc/grpc-java/issues/1901")
  @java.lang.Deprecated // Use {@link #getExecMethod()} instead. 
  public static final io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteEvent,
      com.facebook.buck.worker.model.ExecuteResponse> METHOD_EXEC = getExecMethodHelper();

  private static volatile io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteEvent,
      com.facebook.buck.worker.model.ExecuteResponse> getExecMethod;

  @io.grpc.ExperimentalApi("https://github.com/grpc/grpc-java/issues/1901")
  public static io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteEvent,
      com.facebook.buck.worker.model.ExecuteResponse> getExecMethod() {
    return getExecMethodHelper();
  }

  private static io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteEvent,
      com.facebook.buck.worker.model.ExecuteResponse> getExecMethodHelper() {
    io.grpc.MethodDescriptor<com.facebook.buck.worker.model.ExecuteEvent, com.facebook.buck.worker.model.ExecuteResponse> getExecMethod;
    if ((getExecMethod = WorkerGrpc.getExecMethod) == null) {
      synchronized (WorkerGrpc.class) {
        if ((getExecMethod = WorkerGrpc.getExecMethod) == null) {
          WorkerGrpc.getExecMethod = getExecMethod = 
              io.grpc.MethodDescriptor.<com.facebook.buck.worker.model.ExecuteEvent, com.facebook.buck.worker.model.ExecuteResponse>newBuilder()
              .setType(io.grpc.MethodDescriptor.MethodType.CLIENT_STREAMING)
              .setFullMethodName(generateFullMethodName(
                  "worker.Worker", "Exec"))
              .setSampledToLocalTracing(true)
              .setRequestMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  com.facebook.buck.worker.model.ExecuteEvent.getDefaultInstance()))
              .setResponseMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  com.facebook.buck.worker.model.ExecuteResponse.getDefaultInstance()))
                  .setSchemaDescriptor(new WorkerMethodDescriptorSupplier("Exec"))
                  .build();
          }
        }
     }
     return getExecMethod;
  }

  /**
   * Creates a new async stub that supports all call types for the service
   */
  public static WorkerStub newStub(io.grpc.Channel channel) {
    return new WorkerStub(channel);
  }

  /**
   * Creates a new blocking-style stub that supports unary and streaming output calls on the service
   */
  public static WorkerBlockingStub newBlockingStub(
      io.grpc.Channel channel) {
    return new WorkerBlockingStub(channel);
  }

  /**
   * Creates a new ListenableFuture-style stub that supports unary calls on the service
   */
  public static WorkerFutureStub newFutureStub(
      io.grpc.Channel channel) {
    return new WorkerFutureStub(channel);
  }

  /**
   */
  public static abstract class WorkerImplBase implements io.grpc.BindableService {

    /**
     * <pre>
     * TODO(ctolliday) delete once workers switch to Exec
     * </pre>
     */
    public void execute(com.facebook.buck.worker.model.ExecuteCommand request,
        io.grpc.stub.StreamObserver<com.facebook.buck.worker.model.ExecuteResponse> responseObserver) {
      asyncUnimplementedUnaryCall(getExecuteMethodHelper(), responseObserver);
    }

    /**
     */
    public io.grpc.stub.StreamObserver<com.facebook.buck.worker.model.ExecuteEvent> exec(
        io.grpc.stub.StreamObserver<com.facebook.buck.worker.model.ExecuteResponse> responseObserver) {
      return asyncUnimplementedStreamingCall(getExecMethodHelper(), responseObserver);
    }

    @java.lang.Override public final io.grpc.ServerServiceDefinition bindService() {
      return io.grpc.ServerServiceDefinition.builder(getServiceDescriptor())
          .addMethod(
            getExecuteMethodHelper(),
            asyncUnaryCall(
              new MethodHandlers<
                com.facebook.buck.worker.model.ExecuteCommand,
                com.facebook.buck.worker.model.ExecuteResponse>(
                  this, METHODID_EXECUTE)))
          .addMethod(
            getExecMethodHelper(),
            asyncClientStreamingCall(
              new MethodHandlers<
                com.facebook.buck.worker.model.ExecuteEvent,
                com.facebook.buck.worker.model.ExecuteResponse>(
                  this, METHODID_EXEC)))
          .build();
    }
  }

  /**
   */
  public static final class WorkerStub extends io.grpc.stub.AbstractStub<WorkerStub> {
    private WorkerStub(io.grpc.Channel channel) {
      super(channel);
    }

    private WorkerStub(io.grpc.Channel channel,
        io.grpc.CallOptions callOptions) {
      super(channel, callOptions);
    }

    @java.lang.Override
    protected WorkerStub build(io.grpc.Channel channel,
        io.grpc.CallOptions callOptions) {
      return new WorkerStub(channel, callOptions);
    }

    /**
     * <pre>
     * TODO(ctolliday) delete once workers switch to Exec
     * </pre>
     */
    public void execute(com.facebook.buck.worker.model.ExecuteCommand request,
        io.grpc.stub.StreamObserver<com.facebook.buck.worker.model.ExecuteResponse> responseObserver) {
      asyncUnaryCall(
          getChannel().newCall(getExecuteMethodHelper(), getCallOptions()), request, responseObserver);
    }

    /**
     */
    public io.grpc.stub.StreamObserver<com.facebook.buck.worker.model.ExecuteEvent> exec(
        io.grpc.stub.StreamObserver<com.facebook.buck.worker.model.ExecuteResponse> responseObserver) {
      return asyncClientStreamingCall(
          getChannel().newCall(getExecMethodHelper(), getCallOptions()), responseObserver);
    }
  }

  /**
   */
  public static final class WorkerBlockingStub extends io.grpc.stub.AbstractStub<WorkerBlockingStub> {
    private WorkerBlockingStub(io.grpc.Channel channel) {
      super(channel);
    }

    private WorkerBlockingStub(io.grpc.Channel channel,
        io.grpc.CallOptions callOptions) {
      super(channel, callOptions);
    }

    @java.lang.Override
    protected WorkerBlockingStub build(io.grpc.Channel channel,
        io.grpc.CallOptions callOptions) {
      return new WorkerBlockingStub(channel, callOptions);
    }

    /**
     * <pre>
     * TODO(ctolliday) delete once workers switch to Exec
     * </pre>
     */
    public com.facebook.buck.worker.model.ExecuteResponse execute(com.facebook.buck.worker.model.ExecuteCommand request) {
      return blockingUnaryCall(
          getChannel(), getExecuteMethodHelper(), getCallOptions(), request);
    }
  }

  /**
   */
  public static final class WorkerFutureStub extends io.grpc.stub.AbstractStub<WorkerFutureStub> {
    private WorkerFutureStub(io.grpc.Channel channel) {
      super(channel);
    }

    private WorkerFutureStub(io.grpc.Channel channel,
        io.grpc.CallOptions callOptions) {
      super(channel, callOptions);
    }

    @java.lang.Override
    protected WorkerFutureStub build(io.grpc.Channel channel,
        io.grpc.CallOptions callOptions) {
      return new WorkerFutureStub(channel, callOptions);
    }

    /**
     * <pre>
     * TODO(ctolliday) delete once workers switch to Exec
     * </pre>
     */
    public com.google.common.util.concurrent.ListenableFuture<com.facebook.buck.worker.model.ExecuteResponse> execute(
        com.facebook.buck.worker.model.ExecuteCommand request) {
      return futureUnaryCall(
          getChannel().newCall(getExecuteMethodHelper(), getCallOptions()), request);
    }
  }

  private static final int METHODID_EXECUTE = 0;
  private static final int METHODID_EXEC = 1;

  private static final class MethodHandlers<Req, Resp> implements
      io.grpc.stub.ServerCalls.UnaryMethod<Req, Resp>,
      io.grpc.stub.ServerCalls.ServerStreamingMethod<Req, Resp>,
      io.grpc.stub.ServerCalls.ClientStreamingMethod<Req, Resp>,
      io.grpc.stub.ServerCalls.BidiStreamingMethod<Req, Resp> {
    private final WorkerImplBase serviceImpl;
    private final int methodId;

    MethodHandlers(WorkerImplBase serviceImpl, int methodId) {
      this.serviceImpl = serviceImpl;
      this.methodId = methodId;
    }

    @java.lang.Override
    @java.lang.SuppressWarnings("unchecked")
    public void invoke(Req request, io.grpc.stub.StreamObserver<Resp> responseObserver) {
      switch (methodId) {
        case METHODID_EXECUTE:
          serviceImpl.execute((com.facebook.buck.worker.model.ExecuteCommand) request,
              (io.grpc.stub.StreamObserver<com.facebook.buck.worker.model.ExecuteResponse>) responseObserver);
          break;
        default:
          throw new AssertionError();
      }
    }

    @java.lang.Override
    @java.lang.SuppressWarnings("unchecked")
    public io.grpc.stub.StreamObserver<Req> invoke(
        io.grpc.stub.StreamObserver<Resp> responseObserver) {
      switch (methodId) {
        case METHODID_EXEC:
          return (io.grpc.stub.StreamObserver<Req>) serviceImpl.exec(
              (io.grpc.stub.StreamObserver<com.facebook.buck.worker.model.ExecuteResponse>) responseObserver);
        default:
          throw new AssertionError();
      }
    }
  }

  private static abstract class WorkerBaseDescriptorSupplier
      implements io.grpc.protobuf.ProtoFileDescriptorSupplier, io.grpc.protobuf.ProtoServiceDescriptorSupplier {
    WorkerBaseDescriptorSupplier() {}

    @java.lang.Override
    public com.google.protobuf.Descriptors.FileDescriptor getFileDescriptor() {
      return com.facebook.buck.worker.model.WorkerProto.getDescriptor();
    }

    @java.lang.Override
    public com.google.protobuf.Descriptors.ServiceDescriptor getServiceDescriptor() {
      return getFileDescriptor().findServiceByName("Worker");
    }
  }

  private static final class WorkerFileDescriptorSupplier
      extends WorkerBaseDescriptorSupplier {
    WorkerFileDescriptorSupplier() {}
  }

  private static final class WorkerMethodDescriptorSupplier
      extends WorkerBaseDescriptorSupplier
      implements io.grpc.protobuf.ProtoMethodDescriptorSupplier {
    private final String methodName;

    WorkerMethodDescriptorSupplier(String methodName) {
      this.methodName = methodName;
    }

    @java.lang.Override
    public com.google.protobuf.Descriptors.MethodDescriptor getMethodDescriptor() {
      return getServiceDescriptor().findMethodByName(methodName);
    }
  }

  private static volatile io.grpc.ServiceDescriptor serviceDescriptor;

  public static io.grpc.ServiceDescriptor getServiceDescriptor() {
    io.grpc.ServiceDescriptor result = serviceDescriptor;
    if (result == null) {
      synchronized (WorkerGrpc.class) {
        result = serviceDescriptor;
        if (result == null) {
          serviceDescriptor = result = io.grpc.ServiceDescriptor.newBuilder(SERVICE_NAME)
              .setSchemaDescriptor(new WorkerFileDescriptorSupplier())
              .addMethod(getExecuteMethodHelper())
              .addMethod(getExecMethodHelper())
              .build();
        }
      }
    }
    return result;
  }
}
