# Configuring a bootstrapping toolchain setup

This project provides an example of what it might look like to configure a bootstrapping toolchain and construct a different toolchain using an artifact built with the former.

## How to build

1. Build or install `buck2` with Cargo
2. This project assumes Rust, Clang, and Python to be present. See `toolchains/BUCK` for how we pull those in from the system.
3. Run `buck2 init --git`
4. Run commands: e.g. `buck2 run :hello_world`, `buck2 build //...`

## Project setup

### Bootstrap constraint

In order to differentiate between a regular toolchain and a bootstrap toolchain, we introduce a new constraint setting `bootstrap//:bootstrap` and a corresponding constraint value `bootstrap//:use_bootstrap`.

```python
constraint_setting(
    name = "bootstrap",
)

constraint_value(
    name = "use_bootstrap",
    constraint_setting = ":bootstrap",
)
```

### Bootstrap platform

We then define a new platform `bootstrap//platform:bootstrap`, which inherits everything from the default platform `bootstrap//platform:default` and adds the extra `bootstrap` constraint defined above.

```python
platform(
  name = "bootstrap",
  deps = [":default"],
  constraint_values = ["bootstrap//:use_bootstrap"],
)
```

### Bootstrap toolchain

We are using Rust for this example, but the concept is not specific to Rust. Our goal is to
build a Rust compiler with the bootstrap toolchain, construct a new toolchain with the compiler,
then build a Rust binary with the newly built Rust compiler. For simplicity, we are not building
an actual Rust compiler, but using a small wrapper Rust binary that execs into the compiler picked from the system.

First, we setup a bootstrap toolchain using the `system_rust_toolchain` provided in the prelude.
```python
system_rust_toolchain(
    name = "rust_bootstrap_toolchain",
)
```

Then, we configure a build for our "rustc".
```python
rust_binary(
    name = "rustc_wrapper",
    srcs = ["rustc_wrapper.rs"],
)
```

To construct a new toolchain that uses the new "rustc", we use `configured_alias` to tack on the `bootstrap` to the binary.
```python
rust_toolchain(
    name = "rust_toolchain_with_compiled_rustc",
    compiler = ":rustc_with_bootstrap_toolchain",
)

configured_alias(
    name = "rustc_with_bootstrap_toolchain",
    actual = ":rustc_wrapper",
    platform = "bootstrap//platform:bootstrap"
)
```

Now that we have both toolchains constructed, we can create our final Rust toolchain that switches between the two based on the `use_bootstrap` constraint.
```python
toolchain_alias(
   name = "rust",
   actual = select({
     "bootstrap//:use_bootstrap": ":rust_bootstrap_toolchain",
     "DEFAULT": ":rust_toolchain_with_compiled_rustc",
   }),
   visibility = ["PUBLIC"],
)
```
