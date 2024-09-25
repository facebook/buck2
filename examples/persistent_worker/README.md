# Persistent Worker Demo

At the time of writing (2024-09-25) Buck2 supports persistent workers
for local builds through a dedicated Buck2 persistent worker gRPC
protocol. However, Buck2 does not support persistent workers for builds
that use remote execution. This demo is part of a patch-set that adds
support for remote persistent workers to Buck2, see [#776].

[#776]: https://github.com/facebook/buck2/issues/776

## Requirements

The demo uses Nix to provide required system dependencies, such as
Python and grpc-io. As well as direnv to set up the environment.

- [Nix](https://nixos.org/)
- [direnv](https://direnv.net/)

Credentials for [BuildBuddy](https://www.buildbuddy.io/) and
[GHCR](https://ghcr.io) stored in `.envrc.private`:
```
export BUILDBUDDY_API_KEY=...
export BUILDBUDDY_CONTAINER_USER=...  # GitHub user name
export BUILDBUDDY_CONTAINER_PASSWORD=...  # GitHub access token
export GITHUB_USER=... # GitHub user name
```

Upload the Nix Docker image for remote execution as described in
`platforms/buildbuddy.bzl`.

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
$ echo '<file:.buckconfig.buildbuddy>' > .buckconfig.local
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
