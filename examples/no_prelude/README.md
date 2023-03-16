## No-prelude example
This is an example project that does not rely on https://github.com/facebook/buck2-prelude. Instead the prelude cell points to the project root where there is an empty `prelude.bzl` file, like so:
```
#.buckconfig
[repositories]
root = .
prelude = .
...
```

All rules and toolchains are defined manually within each of the subdirectories. (e.g. `cpp/rules.bzl`, `cpp/toolchain.bzl`)

## Sample commands
Install Buck2, cd into a project, and run
```bash
# List all targets
buck2 targets //...
# Build all targets
buck2 build //...
# Run C++ hello_world main
buck2 run //cpp/hello_world:main
```
