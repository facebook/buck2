# Advanced Buck2 Rule Patterns

Common patterns and best practices for writing production-ready Buck2 rules.

## Table of Contents

1. [Custom Providers](#custom-providers)
2. [Transitive Dependencies](#transitive-dependencies)
3. [Toolchain Dependencies](#toolchain-dependencies)
4. [Dynamic Output Names](#dynamic-output-names)
5. [Multiple Outputs](#multiple-outputs)
6. [Command Line Building](#command-line-building)
7. [Configuration-Dependent Rules](#configuration-dependent-rules)
8. [Testing Rules](#testing-rules)

---

## Custom Providers

### Pattern: Library with Transitive Headers

Libraries need to expose headers to dependents:

```starlark
# Define provider for library information
CxxLibraryInfo = provider(fields = {
    "headers": provider_field(typing.Any),            # Direct headers
    "transitive_headers": provider_field(typing.Any), # All headers (including deps)
    "objects": provider_field(typing.Any),            # Compiled objects
    "link_flags": provider_field(typing.Any),         # Linker flags
})

def cxx_library_impl(ctx: AnalysisContext) -> list[Provider]:
    # Compile source files
    objects = []
    for src in ctx.attrs.srcs:
        obj = ctx.actions.declare_output(src.short_path + ".o")
        ctx.actions.run(cmd_args([
            "g++", "-c", src, "-o", obj.as_output()
        ]))
        objects.append(obj)

    # Collect transitive headers from dependencies
    transitive_headers = []
    transitive_headers.extend(ctx.attrs.headers)  # Our own headers

    for dep in ctx.attrs.deps:
        if CxxLibraryInfo in dep:
            dep_info = dep[CxxLibraryInfo]
            transitive_headers.extend(dep_info.transitive_headers)

    # Create archive
    archive = ctx.actions.declare_output("lib" + ctx.label.name + ".a")
    ctx.actions.run(cmd_args(["ar", "rcs", archive.as_output()] + objects))

    return [
        DefaultInfo(default_output = archive),
        CxxLibraryInfo(
            headers = ctx.attrs.headers,
            transitive_headers = transitive_headers,
            objects = objects,
            link_flags = [],
        ),
    ]

cxx_library = rule(
    impl = cxx_library_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "headers": attrs.list(attrs.source()),
        "deps": attrs.list(attrs.dep()),
    },
)
```

**Usage in binary rule:**

```starlark
def cxx_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    # Collect all headers from dependencies
    all_headers = []
    all_archives = []

    for dep in ctx.attrs.deps:
        if CxxLibraryInfo in dep:
            lib_info = dep[CxxLibraryInfo]
            all_headers.extend(lib_info.transitive_headers)
            all_archives.append(dep[DefaultInfo].default_outputs[0])

    # Compile with all headers available
    # Link with all archives
    # ...
```

---

## Transitive Dependencies

### Pattern: Collecting Dependencies Recursively

Often you need to collect information from all transitive dependencies:

```starlark
def collect_transitive_deps(deps, provider_type, field_name):
    """Helper to collect a field transitively from dependencies."""
    result = []
    for dep in deps:
        if provider_type in dep:
            provider = dep[provider_type]
            field_value = getattr(provider, field_name)
            result.extend(field_value)
    return result

def my_rule_impl(ctx: AnalysisContext):
    # Collect all transitive shared libraries
    transitive_libs = collect_transitive_deps(
        ctx.attrs.deps,
        MyLibraryInfo,
        "transitive_shared_libs"
    )

    # Add our own
    transitive_libs.append(my_lib)

    return [
        MyLibraryInfo(
            transitive_shared_libs = transitive_libs,
        ),
    ]
```

### Pattern: Deduplicated Transitive Dependencies

Use `dedupe()` to avoid duplicates:

```starlark
def my_rule_impl(ctx: AnalysisContext):
    all_libs = []

    for dep in ctx.attrs.deps:
        if MyLibraryInfo in dep:
            all_libs.extend(dep[MyLibraryInfo].transitive_libs)

    # Deduplicate while preserving order
    all_libs = dedupe(all_libs)

    return [MyLibraryInfo(transitive_libs = all_libs)]
```

### ⚠️ Important: Use Transitive Sets in Production

**The manual collection patterns above are simple but inefficient.** In most
cases, you should use **transitive sets** (`tset`) instead. Transitive sets are
Buck2's optimized data structure for propagating information up dependency
trees.

**Why transitive sets?**

- **Memory efficient:** Low cost of creation and memory usage in Starlark
- **Execution efficient:** Edges can be shared instead of duplicating data
- **Automatic deduplication:** Handles DAG traversal correctly
- **Lazy evaluation:** Projections are computed only when needed

**Basic usage:**

```starlark
# 1. Declare the transitive set type with a projection
def _project_as_args(value: str):
    return cmd_args(value, format = "-I{}")

MyHeaderSet = transitive_set(
    args_projections = {"include_dirs": _project_as_args}
)

# 2. Create sets in your rule implementation
def my_library_impl(ctx: AnalysisContext):
    # Collect children sets from dependencies
    children = [
        dep[MyLibraryInfo].headers_tset
        for dep in ctx.attrs.deps
        if MyLibraryInfo in dep
    ]

    # Create our set with our value and children
    headers_tset = ctx.actions.tset(
        MyHeaderSet,
        value = ctx.attrs.include_dir,  # Our contribution
        children = children,             # Transitive dependencies
    )

    return [
        MyLibraryInfo(headers_tset = headers_tset),
    ]

# 3. Use the projection in command lines
def my_binary_impl(ctx: AnalysisContext):
    all_includes = ctx.attrs.deps[MyLibraryInfo].headers_tset.project_as_args("include_dirs")

    cmd = cmd_args(["gcc", ctx.attrs.src, all_includes, "-o", output.as_output()])
    ctx.actions.run(cmd)
```

**Key points:**

- Creating projections is **very cheap** (independent of set size)
- Projections can be used in command lines (`project_as_args`) or JSON
  (`project_as_json`)
- Avoid iterating over tsets (`traverse()`) unless absolutely necessary - use
  projections instead
- Different traversal orders available: `preorder`, `postorder`, `topological`,
  `bfs`

**When to use manual collection vs tsets:**

- **Use tsets:** For transitive dependencies (headers, link flags, libraries,
  etc.) - this is the recommended approach
- **Use manual collection:** Only for simple cases or when learning Buck2 basics

For complete details, see the Buck2 documentation on transitive sets
(https://buck2.build/docs/rule_authors/transitive_sets/).

---

## Toolchain Dependencies

### Pattern: Using Toolchains

Toolchains provide compiler/tool information. A complete toolchain
implementation consists of three parts:

**1. Define the toolchain info provider:**

```starlark
# In rust_toolchain.bzl
RustToolchainInfo = provider(fields = {
    "compiler": provider_field(typing.Any),  # RunInfo for rustc
    "rustc_flags": provider_field(typing.Any), # Default compiler flags
})
```

**2. Define the toolchain rule:**

```starlark
# In rust_toolchain.bzl
def _rust_toolchain_impl(ctx: AnalysisContext):
    return [
        DefaultInfo(),
        RustToolchainInfo(
            compiler = ctx.attrs.compiler[RunInfo],
            rustc_flags = ctx.attrs.rustc_flags,
        ),
    ]

rust_toolchain = rule(
    impl = _rust_toolchain_impl,
    attrs = {
        "compiler": attrs.dep(providers = [RunInfo]),
        "rustc_flags": attrs.list(attrs.arg(), default = []),
    },
    is_toolchain_rule = True,  # Mark this as a toolchain rule
)
```

**3. Create a toolchain target in BUCK:**

```starlark
# In //toolchains/BUCK
rust_toolchain(
    name = "rust",
    compiler = ":rustc_wrapper",
    rustc_flags = ["-C", "opt-level=2"],
)

# Helper target that provides RunInfo for rustc
command_alias(
    name = "rustc_wrapper",
    exe = "/usr/bin/rustc",  # System rustc
)
```

**4. Use the toolchain in your rule:**

```starlark
def rust_binary_impl(ctx: AnalysisContext):
    # Access toolchain
    toolchain = ctx.attrs._toolchain[RustToolchainInfo]

    output = ctx.actions.declare_output(ctx.label.name)

    # Build command using toolchain
    cmd = cmd_args()
    cmd.add(toolchain.compiler)        # Add rustc from toolchain
    cmd.add(toolchain.rustc_flags)     # Add flags from toolchain
    cmd.add("--crate-type=bin")
    cmd.add(ctx.attrs.src)
    cmd.add("-o", output.as_output())

    ctx.actions.run(cmd, category = "rustc")

    return [DefaultInfo(default_output = output)]

rust_binary = rule(
    impl = rust_binary_impl,
    attrs = {
        "src": attrs.source(),
        "_toolchain": attrs.toolchain_dep(
            default = "//toolchains:rust",  # Default toolchain target
        ),
    },
)
```

**Key points:**

- Toolchain rules must set `is_toolchain_rule = True`
- Toolchain rules return a custom provider (e.g., `RustToolchainInfo`) with tool
  information
- Consumer rules access toolchains via `attrs.toolchain_dep()` (usually as a
  private `_toolchain` attribute)
- This pattern allows different toolchains (e.g., stable vs nightly Rust)
  without changing rule implementations

---

## Multiple Outputs

### Pattern: Output Directory

When a tool produces multiple files, use an output directory:

```starlark
def my_rule_impl(ctx: AnalysisContext):
    # Declare output directory
    output_dir = ctx.actions.declare_output("outputs", dir = True)

    # Command writes multiple files to directory
    cmd = cmd_args([
        "my_tool",
        "--output-dir", output_dir.as_output(),
    ])

    ctx.actions.run(cmd)

    return [DefaultInfo(default_output = output_dir)]
```

### Pattern: Main Output + Sub-Outputs

```starlark
def compiler_impl(ctx: AnalysisContext):
    # Main output: executable
    exe = ctx.actions.declare_output(ctx.label.name)

    # Debug symbols as sub-output
    debug_symbols = ctx.actions.declare_output(ctx.label.name + ".debug")

    # Compilation database as sub-output
    compile_commands = ctx.actions.declare_output("compile_commands.json")

    ctx.actions.run(cmd_args([
        "my_compiler",
        ctx.attrs.src,
        "-o", exe.as_output(),
        "--debug-output", debug_symbols.as_output(),
        "--compilation-database", compile_commands.as_output(),
    ]))

    return [
        DefaultInfo(
            default_output = exe,
            sub_targets = {
                "debug": [DefaultInfo(default_output = debug_symbols)],
                "compdb": [DefaultInfo(default_output = compile_commands)],
            },
        ),
    ]

# Build different outputs:
# buck2 build //:app              # Main executable
# buck2 build //:app[debug]       # Debug symbols
# buck2 build //:app[compdb]      # Compilation database
```

---

## Command Line Building

### Pattern: Complex Command Lines

```starlark
def my_rule_impl(ctx: AnalysisContext):
    cmd = cmd_args()

    # Add tool
    cmd.add(ctx.attrs.toolchain[MyToolInfo].tool)

    # Add flags
    cmd.add("-Wall", "-O2")

    # Add user flags
    cmd.add(ctx.attrs.flags)

    # Add source files
    cmd.add(ctx.attrs.srcs)

    # Add include directories (prepend -I to each)
    for include in ctx.attrs.includes:
        cmd.add("-I")
        cmd.add(include)

    # Or more concisely:
    cmd.add(cmd_args(ctx.attrs.includes, format = "-I{}"))

    # Add output
    cmd.add("-o", output.as_output())

    # Hidden inputs (for dependencies but not in command)
    cmd.add(hidden = [dep_artifact1, dep_artifact2])

    ctx.actions.run(cmd)
```

### Pattern: Conditional Command Line Arguments

```starlark
def my_rule_impl(ctx: AnalysisContext):
    cmd = cmd_args(["my_tool"])

    # Add flags conditionally
    if ctx.attrs.debug:
        cmd.add("-g")

    if ctx.attrs.optimize:
        cmd.add("-O3")
    else:
        cmd.add("-O0")

    # Platform-specific flags
    if ctx.attrs._target_os_type[OsLookup].platform == "windows":
        cmd.add("-DWINDOWS")

    ctx.actions.run(cmd)
```

---

## Configuration-Dependent Rules

### Pattern: Using select() in Rules

```starlark
# In BUCK file, users can use select():
my_rule(
    name = "app",
    srcs = ["main.cpp"],
    flags = select({
        "ovr_config//os:linux": ["-DLINUX"],
        "ovr_config//os:macos": ["-DMACOS"],
        "DEFAULT": [],
    }),
)

# In rule implementation, just use the value:
def my_rule_impl(ctx: AnalysisContext):
    # ctx.attrs.flags is already resolved to the correct value
    cmd = cmd_args(["compiler"] + ctx.attrs.flags + [ctx.attrs.src])
    # ...
```

---

## Testing Rules

### Pattern: Test Rule with Runner

```starlark
def my_test_impl(ctx: AnalysisContext):
    # Build the test binary (same as binary rule)
    test_exe = ctx.actions.declare_output(ctx.label.name)
    ctx.actions.run(cmd_args(["gcc", ctx.attrs.src, "-o", test_exe.as_output()]))

    # Create test runner script
    runner = ctx.actions.write(
        "run_test.sh",
        [
            "#!/bin/bash",
            "set -e",
            "exec " + cmd_args(test_exe).relative_to(ctx.label.path),
        ],
    )

    return [
        DefaultInfo(default_output = test_exe),
        RunInfo(args = cmd_args([runner])),
        ExternalRunnerTestInfo(
            type = "custom",
            command = [runner],
        ),
    ]

my_test = rule(
    impl = my_test_impl,
    attrs = {
        "src": attrs.source(),
    },
)
```

**Run the test:**

```bash
buck2 test //:my_test
```

### Pattern: Test with Test Data

```starlark
def my_test_impl(ctx: AnalysisContext):
    test_exe = ...  # Build test executable

    # Test needs access to data files
    test_data = ctx.attrs.data

    # Create a test runner that sets up the environment
    runner = ctx.actions.write_json(
        "test_runner.json",
        {
            "exe": test_exe,
            "data": test_data,
        },
    )

    return [
        DefaultInfo(default_output = test_exe),
        RunInfo(
            args = cmd_args([test_exe], hidden = test_data)
        ),
        ExternalRunnerTestInfo(
            type = "custom",
            command = [test_exe],
            env = {
                "TEST_DATA": cmd_args(test_data, delimiter = ","),
            },
        ),
    ]

my_test = rule(
    impl = my_test_impl,
    attrs = {
        "src": attrs.source(),
        "data": attrs.list(attrs.source(), default = []),
    },
)
```

---

## Common Patterns Summary

| Pattern                 | Use Case                                                       |
| ----------------------- | -------------------------------------------------------------- |
| Custom Providers        | Share information between rules                                |
| Transitive Dependencies | Collect headers, libraries, etc. from all deps                 |
| **Transitive Sets**     | **Efficient transitive propagation (use this in production!)** |
| Toolchains              | Abstract compiler/tool locations                               |
| Multiple Outputs        | Output directories, sub-outputs, debug info                    |
| Command Building        | Construct complex command lines                                |
| Configuration-Dependent | Platform-specific behavior                                     |
| Test Rules              | Executable tests with data                                     |

These patterns form the building blocks of production Buck2 rules. Combine them
as needed for your specific use case.

**Note:** For production code, prefer transitive sets over manual dependency
collection for better performance and correctness.
