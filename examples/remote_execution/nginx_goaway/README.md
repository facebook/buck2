# Remote cache behind an nginx gRPC proxy

This project builds against [bazel-remote](https://github.com/buchgr/bazel-remote)
behind an nginx gRPC proxy, and checks that buck2 copes with the HTTP/2
`GOAWAY` frames such proxies send. It is a trimmed down version of
[arianvp/buck2-grpc-reproducer](https://github.com/arianvp/buck2-grpc-reproducer)
for [#1294](https://github.com/facebook/buck2/issues/1294).

L7 load balancers like nginx or AWS ALB close long-lived HTTP/2 connections
gracefully: after a number of requests (nginx's `keepalive_requests`, 1000 by
default) they send `GOAWAY` with `NO_ERROR`, finish the requests they already
accepted, and close the connection. Requests the proxy did not accept were
never processed, so a client should retry them on a new connection rather than
fail the build with errors like:

```
Failed to make BatchReadBlobs request: code: 'Internal error', message: "h2 protocol error: http2 error",
source: tonic::transport::Error(Transport, hyper::Error(Http2, Error { kind: GoAway(b"", NO_ERROR, Remote) }))
```

`nginx.conf` sends `GOAWAY` every 100 requests so that each build runs into it
many times. Most targets write tiny outputs, whose requests the gRPC transport
retries by itself. A hundred write 512 KiB, more than the transport buffers to
replay a request, so it fails those uploads with `UNAVAILABLE` and buck2 has to
retry them on its own.

## Running it

You need `buck2`, `nginx` (1.19.7 or newer) and `bazel-remote` on `$PATH`, or
point `$BUCK2`, `$NGINX` and `$BAZEL_REMOTE` at them. For example, using Nix
for nginx and bazel-remote:

```sh
BUCK2=/path/to/buck2 nix shell nixpkgs#nginx nixpkgs#bazel-remote --command ./test.sh
```

`test.sh` starts bazel-remote on port 19092 and nginx on port 18443, then
builds every target twice: once with an empty cache, which runs the actions
locally and uploads the results, and once more after `buck2 clean`, which
downloads them. It fails if either build fails, if any action in the second
build was not a cache hit, or if nginx never sent a `GOAWAY`.
