# Remote execution integration with NativeLink

This project provides a small example of what a project that utilizes
[NativeLink](https://github.com/Tracemachina/nativelink).

In this document, we will go over the key configs used in this setup. If you
already have a `NativeLink` deployment you can use that instead.

## Deploy a local NativeLink

### 📦 Installing with Cargo

1. First install Rust, but skip to step 2 if you have it already.

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

2. Install NativeLink with Cargo.

```bash
cargo install --git https://github.com/TraceMachina/nativelink --tag v0.4.0
```

### ⚙️ Configure and 🦾 Start NativeLink

The `nativelink` executable reads a JSON file as it's only parameter,
`--config`. See
[`nativelink-config`](https://github.com/TraceMachina/nativelink/tree/main/nativelink-config/examples/basic_cas.json)
for more details and examples.

To grab the example in your current working directory, run:

```bash
curl -O https://raw.githubusercontent.com/TraceMachina/nativelink/main/nativelink-config/examples/basic_cas.json

### you can modify the example above to replace the filesystem store with the memory store if you favor speed over data durability.
nativelink basic_cas.json
```

More information is available in the
[repo](https://github.com/Tracemachina/nativelink).

## Relevant configs in .buckconfig

Configure the `NativeLink` endpoint as follows:

```ini
[buck2_re_client]
action_cache_address = grpc://localhost:50051
engine_address = grpc://localhost:50051
cas_address = grpc://localhost:50051
tls = false
instance_name = main
```

TLS is not used in this example.

## Relevant configs in `ExecutionPlatformInfo`

`NativeLink` takes in a Docker image and `OSFamily` in its RE properties to
select a worker. This is configured in `root//platforms:platforms`.
