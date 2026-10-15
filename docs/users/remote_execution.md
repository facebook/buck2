---
id: remote_execution
title: Remote Execution
---

Buck2 can use services that expose
[Bazel's remote execution API](https://github.com/bazelbuild/remote-apis) in
order to run actions remotely.

Buck2 projects have been successfully tested for remote execution against
[EngFlow](https://www.engflow.com/),
[BuildBarn](https://github.com/buildbarn/bb-remote-execution) and
[BuildBuddy](https://www.buildbuddy.io). Sample project configurations for those
providers are available under
[examples/remote_execution](https://github.com/facebook/buck2/tree/main/examples/remote_execution).

## RE configuration in `.buckconfig`

Configuration for remote execution can be found under `[buck2_re_client]` in
`.buckconfig`.

Keys supported include:

- `engine_address` - address to your RE's engine.
- `action_cache_address` - address to your action cache endpoint.
- `cas_address` - address to your content-addressable storage (CAS) endpoint.
  - Supported schemes: `grpc://`, `grpcs://`, `http://`, `https://`, and gRPC
    resolver schemes (`dns://`, `ipv4://`, `ipv6://`).
  - If no scheme is provided, Buck2 treats the endpoint as TLS-enabled.
- `tls_ca_certs` - path to a CA certificates bundle. This must be PEM-encoded.
  If set, this replaces the default trust roots. If none is set, a default
  bundle will be used. This path contains environment variables using shell
  interpolation syntax (i.e. $VAR). They will be substituted before reading the
  file.
- `tls_client_cert` - path to a client certificate (and intermediate chain), as
  well as its associated private key. This must be PEM-encoded. This path can
  contain environment variables using shell interpolation syntax (i.e. $VAR).
  They will be substituted before reading the file.
- `http_headers` - HTTP headers to inject in all requests to RE. This is a
  comma-separated list of `Header: Value` pairs. Minimal validation of those
  headers is done here. This can contain environment variables using shell
  interpolation syntax ($VAR). They will be substituted before reading the file.
- `instance_name` - an instance name to pass on execution, action cache, and CAS
  requests.
- `capabilities` - whether Buck2 should query the RE capabilities service. This
  defaults to enabled.
- `max_total_batch_size` - optional client-side cap for cumulative blob size in
  batch CAS requests. Buck2 also honors a smaller server-advertised
  `max_batch_total_size_bytes`.
- `remote_cache_chunking` - enables FastCDC 2020 chunked uploads and downloads
  for large CAS blobs. This requires `capabilities` to be enabled and the server
  to advertise SplitBlob, SpliceBlob, and FastCDC 2020 parameters.
- `remote_cache_chunk_cache_dir` - optional local directory for FastCDC chunk
  blobs. When set with `remote_cache_chunking`, Buck2 checks this directory
  before downloading chunks from remote CAS and writes validated chunks after
  chunked uploads or downloads. The directory is managed by the user.

Buck2 uses `SHA256` for all its hashing by default. If your RE engine requires
something else, this can be configured in `.buckconfig` as follows:

```ini
[buck2]
# Accepts BLAKE3, SHA1, or SHA256
digest_algorithms = BLAKE3
```

When capabilities are enabled, Buck2 records the advertised digest functions,
compressed ByteStream support, action-cache update support, SplitBlob/SpliceBlob
support, FastCDC 2020 chunking parameters, execution priority ranges, and CAS
upload limits in the daemon logs. Buck2 also validates the server's advertised
RE API version range and fails connection setup if there is no compatible
version overlap. If the server advertises `max_cas_blob_size_bytes`, Buck2
rejects larger CAS uploads locally instead of waiting for the server to return
an upload error. Buck2 also checks that the remote cache and enabled remote
execution capabilities advertise the effective `[buck2] digest_algorithms` used
by the daemon. Older servers may omit the remote cache digest function list; in
that case Buck2 warns and assumes SHA256 for remote cache compatibility.
When the server does not advertise enabled remote execution, or when a nonzero
execution priority is outside the advertised supported ranges, Buck2 rejects the
`Execute` request locally.

## RE platform configuration

Next, your build will need an
[execution platform](https://buck2.build/docs/concepts/glossary/#execution-platform)
that specifies how and where actions should be executed. For a sample platform
definition that sets up an execution platform to utilize RE, take a look at the
[EngFlow example](https://github.com/facebook/buck2/blob/main/examples/remote_execution/engflow/platforms/defs.bzl),
[BuildBarn example](https://github.com/facebook/buck2/blob/main/examples/remote_execution/buildbarn/platforms/defs.bzl),
or the
[BuildBuddy example](https://github.com/facebook/buck2/blob/main/examples/remote_execution/buildbuddy/platforms/defs.bzl).

To enable remote execution, configure the following fields in
[CommandExecutorConfig](https://buck2.build/docs/api/build/globals/#commandexecutorconfig)
as follows:

- `remote_enabled` - set to `True`.
- `local_enabled` - set to `True` if you also want to run actions locally.
- `use_limited_hybrid` - set to `False` unless you want to exclusively run
  remotely when possible.
- `remote_execution_properties` - other additional properties.
  - If the RE engine requires a container image, this can be done by setting
    `container-image` to an image URL, as is done in the example above.

## Remote cache policy

Remote-enabled executors can use the same RE backend for action-cache lookups,
dep-file-cache lookups, uploads, and execution. These settings are controlled on
`CommandExecutorConfig`:

- `remote_cache_enabled` - query the remote action cache before executing.
- `remote_dep_file_cache_enabled` - query the remote dep-file cache.
- `allow_cache_uploads` - upload locally produced action results to the remote
  cache.
- `max_cache_upload_mebibytes` - skip remote cache uploads above this size.
- `remote_cache_unavailable_fallback` - treat transient remote cache lookup
  failures as misses and continue with the next executor.

`remote_cache_unavailable_fallback` is intended for availability incidents where
execution can still proceed locally or remotely after a cache read fails. It
applies to cache lookup failures such as unavailable or timed-out cache
requests; non-cache execution failures still follow the executor's normal
fallback policy.

Buck2 also treats a stale action-cache hit as a cache miss when the action-cache
entry exists but one of the referenced output blobs is missing from CAS during
cache materialization. This allows the action to be re-executed instead of
failing the build on the stale cache entry. When this happens, Buck2 remembers
the CAS digests referenced by the stale action result for the lifetime of the
current RE client and ignores later action-cache hits that refer to those
digests. This avoids repeatedly accepting the same stale action-cache entry
after a CAS eviction.

Deferred CAS-backed outputs include their action digest, RE use case, and
expiration metadata. Buck2 refreshes those leases through the RE client before
materializing or re-uploading deferred outputs. If a deferred output is old
enough that CAS can no longer provide it, Buck2 reports the stale digest and
its action-cache origin instead of uploading an incomplete result. This recovery
policy applies to outputs from the current remote-cache result; Buck2 does not
rewind arbitrary already-declared deferred CAS dependencies after they have
expired.

If the server capabilities do not advertise enabled action-cache updates, Buck2
skips local-result cache uploads instead of issuing an unsupported
`UpdateActionResult` RPC. When an upload path asks to upload only missing blobs,
Buck2 checks CAS first and skips blobs that the server already has. Buck2 also
validates uploaded bytes against the digest advertised for local cache uploads
where the RE protocol path exposes the uploaded content to Buck2 before sending
it. Buck2 rejects size or digest mismatches locally rather than writing a
corrupt CAS blob or action-cache result. Buck2 also
rejects malformed `BatchUpdateBlobs` replies and `BatchReadBlobs` replies where
the returned digests do not match the requested batch, so batch cache operations
require a successful response for every requested digest. `FindMissingBlobs`
replies are also checked so a server cannot report unexpected or duplicate
missing digests. Buck2's SplitBlob and SpliceBlob wrappers also validate that
chunk digests can reconstruct the declared blob before those chunk lists are
used by future chunked cache transfers.
