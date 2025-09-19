---
id: toolchains
title: Toolchains
---

# Toolchains

Go toolchains in Buck2 come in two types: "regular" and "bootstrap".

- **Regular toolchains** are used to build Go code.
- **Bootstrap toolchains** are used to build the tools required to build Go
  code.

A minimal setup requires `:go`, `:go_bootstrap`, and `:python_bootstrap` in your
`toolchains//` cell.

Additionally, a `:cxx` toolchain is required:

- If you use CGo, you need a fully configured CXX toolchain
- If you don't use CGo, you can define it with a stub `no_toolchain` rule

Each type of Go toolchain has two implementations: "system" and "hermetic".

## Hermetic Toolchains (Recommended)

Hermetic toolchains are the recommended way to build Go code because they:

- Make builds reproducible
- Support all features of the Go rules

Hermetic toolchains consist of two rules: `go*_distr` and `go*_toolchain`.

The `*_distr` rules wrap Go distributions produced by `go tool dist`, such as
those available at [go.dev/dl](https://go.dev/dl/).

You can use either local or `http_archive` Go distributions.

### Toolchain Locations

- `@prelude//toolchains/go:go_toolchain.bzl`
- `@prelude//toolchains/go:go_bootstrap_toolchain.bzl`

### Example Configuration

Here's an example of `go_toolchain` using local Go distributions
(`go_bootstrap_toolchain` is defined similarly):

```python
load("@prelude//toolchains/go:go_toolchain.bzl", "go_distr", "go_toolchain")

# Note: selects are resolved against execution-platform
go_distr(
    name = "go_distr",
    go_arch = select({
        "config//cpu:arm64": "arm64",
        "config//cpu:x86_64": "amd64",
    }),
    go_os = select({
        "config//os:linux": "linux",
        "config//os:macos": "darwin",
    }),
    go_root = select({
        "config//os:linux": select({"config//cpu:x86_64": "path/to/go-linux-amd64"}),
        "config//os:macos": select({"config//cpu:arm64": "path/to/go-darwin-arm64"}),
    }),
)

# Note: selects are resolved against target-platform
go_toolchain(
    name = "go",
    env_go_arch = select({
        "config//cpu:arm64": "arm64",
        "config//cpu:x86_64": "amd64",
    }),
    env_go_os = select({
        "config//os:linux": "linux",
        "config//os:macos": "darwin",
        "config//os:windows": "windows",
    }),
    go_distr = ":go_distr",
    visibility = ["PUBLIC"],
)
```

For a complete example using `http_archive`, see
[examples/toolchains/go_toolchain](https://github.com/facebook/buck2/blob/main/examples/toolchains/go_toolchain/toolchains/BUCK).

## System Toolchains

System toolchains are not intended for production builds but can be used for
examples and simple projects. Note that some features of Go rules might not work
with system toolchains, such as building ASM files.

### Toolchain Locations

- `@prelude//toolchains/go:system_go_toolchain.bzl`
- `@prelude//toolchains/go:system_go_bootstrap_toolchain.bzl`

### Example Configuration

```python
load("@prelude///toolchains/go:system_go_bootstrap_toolchain.bzl", "system_go_bootstrap_toolchain")
load("@prelude///toolchains/go:system_go_toolchain.bzl", "system_go_toolchain")

system_go_toolchain(
    name = "go",
    visibility = ["PUBLIC"],
)

system_go_bootstrap_toolchain(
    name = "go_bootstrap",
    visibility = ["PUBLIC"],
)
```
