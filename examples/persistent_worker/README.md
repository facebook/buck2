# Persistent Worker Demo

At the time of writing (2024-09-25) Buck2 supports persistent workers for local
builds through a dedicated Buck2 persistent worker gRPC protocol. However, Buck2
does not support persistent workers for builds that use remote execution. This
demo is part of a patch-set that adds support for remote persistent workers to
Buck2, see [#776].

[#776]: https://github.com/facebook/buck2/issues/776

## Requirements

This demo uses BuildBuddy remote execution to demonstrate remote persistent
workers. You will need an API token for at least a free open source account. You
can use [direnv] to set up the environment:

Credentials for [BuildBuddy] stored in `.envrc.private`:

```
export BUILDBUDDY_API_KEY=...
```

On CI the API key is not available for pipelines initiated from forks of the
main Buck2 repository. The corresponding tests will be skipped in that case. A
Meta engineer can manually initiate a pipeline run with the token set.

[direnv]: https://direnv.net/
[BuildBuddy]: https://www.buildbuddy.io/

## Local Build

Configure a local build without persistent workers:

```
$ cd examples/persistent_worker
$ echo '<file:.buckconfig.no-workers>' > .buckconfig.local
```

Run a clean build:

```
$ buck2 clean; buck2 build : -vstderr
...
stderr for root//:demo-7 (demo):
...
ONE-SHOT START
...
```

## Local Persistent Worker

Configure a local build with persistent workers:

```
$ cd examples/persistent_worker
$ echo '<file:.buckconfig.local-persistent-workers>' > .buckconfig.local
```

Run a clean build:

```
$ buck2 clean; buck2 build : -vstderr
...
stderr for root//:demo-7 (demo):
...
Buck2 persistent worker ...
...
```

## Remote Execution

Configure a remote build without persistent workers:

```
$ cd examples/persistent_worker
$ echo '<file:.buckconfig.buildbuddy>' > .buckconfig.local
```

Run a clean build:

```
$ buck2 clean; buck2 build : -vstderr
...
stderr for root//:demo-7 (demo):
...
ONE-SHOT START
...
```

## Remote Persistent Worker

Configure a remote build with persistent workers:

```
$ cd examples/persistent_worker
$ echo '<file:.buckconfig.buildbuddy-persistent-workers>' > .buckconfig.local
```

Run a clean build:

```
$ buck2 clean; buck2 build : -vstderr
...
stderr for root//:demo-7 (demo):
...
Bazel persistent worker ...
...
```

## Protocol

### Starlark

A Buck2 persistent worker is created by a rule that emits the `WorkerInfo`
provider. Setting `remote = True` on this provider indicates that this worker is
remote execution capable.

Buck2 actions indicate that they can utilize a persistent worker by setting the
`exe` parameter to `ctx.actions.run` to `WorkerRunInfo(worker, exe)`, where
`worker` is a `WorkerInfo` provider, and `exe` defines the fallback executable
for non persistent-worker execution.

Buck2 actions that want to utilize a remote persistent worker must pass
command-line arguments in an argument file specified as `@argfile`,
`-flagfile=argfile`, or `--flagfile=argfile` on the command-line.

### Local Persistent Worker

A locally executed Buck2 persistent worker falls under the
[Buck2 persistent worker protocol](./proto/buck2/worker.proto): It is started
and managed by Buck2 and passed a file path in the `WORKER_SOCKET` environment
variable where it should create a gRPC Unix domain socket to serve worker
requests over. Multiple requests may be sent in parallel and expected to be
served at the same time depending on the `concurrency` attribute of the
`WorkerInfo` provider.

### Remote Persistent Worker

A remotely executed Buck2 persistent worker falls under the
[Bazel persistent worker protocol](./proto/bazel/worker_protocol.proto): It is
started and managed by the remote execution system. Work requests are sent as
length prefixed protobuf objects to the standard input of the worker process.
Work responses are expected as length prefixed protobuf objects on the standard
output of the worker process. The worker process may not use standard output for
anything else.
