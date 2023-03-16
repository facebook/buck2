## A simple Hello World project using the buck2-prelude

This example demonstrates how a simple C++ project might be built with Buck2 using the prelude.

In the `toolchains` cell, we define two toolchains needed: `system_cxx_toolchain` and `system_python_bootstrap_toolchain`, both pulled in from the prelude.
The `BUCK` file at the project root contain a `cxx_binary` target and its `cxx_library` dependency.
`.buckconfig` contains the configuration to set the target platform for the project:
```
[parser]
target_platform_detector_spec = target:root//...->prelude//platforms:default
```

## Setup

Run `buck2 init`.

## Sample commands

To view all targets in the project,

```bash
buck2 targets //...
```

To build the main C++ binary,

```bash
buck2 build //:main
```

To run the main C++ binary,

```bash
# Should print "Hello from C++!"
buck2 run //:main
```
