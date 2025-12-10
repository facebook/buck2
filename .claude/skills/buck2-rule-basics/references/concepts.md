# Buck2 Core Concepts - Deep Dive

This reference provides detailed explanations of Buck2 core concepts for users
who want to go beyond the basic tutorial.

## Table of Contents

1. [The Buck2 Build Model](#the-buck2-build-model)
2. [Targets in Depth](#targets-in-depth)
3. [Artifacts](#artifacts)
4. [Actions](#actions)
5. [Providers](#providers)
6. [Configurations](#configurations)
7. [Analysis Phase](#analysis-phase)
8. [Build Graph](#build-graph)

---

## The Buck2 Build Model

Buck2 uses a **declarative, graph-based build model**:

1. **Load Phase**: Read and evaluate BUCK/BUILD files, create unconfigured
   targets
2. **Configuration Phase**: Apply configurations to targets, resolve `select()`
   expressions
3. **Analysis Phase**: Run rule implementations, declare actions and artifacts
4. **Execution Phase**: Run actions to produce artifacts

**Key insight:** Rules don't execute commands - they declare what commands
should be run. Buck2 decides when to actually run them.

---

## Targets in Depth

### Target Terminology

**Target** is an overloaded term in Buck2. Be precise:

#### 1. Unconfigured Target

- **What it is:** A target as written in BUCK files, before configuration is
  applied
- **Identifier:** `//package:name` (no configuration suffix)
- **Query tool:** `buck2 uquery`
- **Characteristics:**
  - Has all attributes from BUCK file
  - `select()` expressions are **not** resolved (still conditional)
  - No platform-specific information applied
  - Same for all configurations

**Example:**

```starlark
# BUCK file
cpp_binary(
    name = "app",
    srcs = select({
        "DEFAULT": ["main.cpp"],
        "ovr_config//os:windows": ["main_windows.cpp"],
    }),
)

# Unconfigured query shows the raw select():
# buck2 uquery //path:app --output-attribute=srcs
# Returns: select({"DEFAULT": ["main.cpp"], ...})
```

#### 2. Configured Target

- **What it is:** A target with a specific configuration applied
- **Identifier:** `//package:name (fbcode//platform/linux:configuration)`
- **Query tool:** `buck2 cquery`
- **Characteristics:**
  - `select()` expressions are resolved to concrete values
  - Platform-specific settings applied (os, cpu, compiler flags, etc.)
  - Different configurations of the same target are distinct build graph nodes

**Example:**

```starlark
# Same target as above, but configured for Linux:
# buck2 cquery //path:app --output-attribute=srcs
# Returns: ["main.cpp"]  (select() resolved to DEFAULT)

# Configured for Windows:
# Returns: ["main_windows.cpp"]  (select() resolved to windows branch)
```

#### 3. Target Label vs Target Pattern

**Target Label:** References a specific target

- With cell: `cell//package:name`
- Without cell: `//package:name` (uses current cell)

**Target Pattern:** Matches multiple targets

- With cell: `cell//package:...` (recursive), `cell//package:*` (package only)
- Without cell: `//package:...` (recursive), `//package:*` (package only)

**Understanding Cells:**

A **cell** is a repository or unit of code in Buck2. Cells allow Buck2 to work
with multiple repositories or isolated parts of a monorepo.

- **Explicit cell reference:** `fbcode//buck2/app:buck2`
  - `fbcode` = cell name
  - `buck2/app` = package path within that cell
  - `buck2` = target name
- **Implicit cell reference:** `//buck2/app:buck2`
  - Uses the current cell based on your working directory
  - Equivalent to `fbcode//buck2/app:buck2` if your current directory is under
    the `fbcode` cell folder (e.g., `/path/to/fbsource/fbcode/`)

**When to use explicit cells:**

- **Cross-cell dependencies:** Depend on targets in a different cell
  - Example: `fbcode//buck2:buck2` depending on `prelude//rules.bzl`
- **Clarity:** Make dependencies explicit, especially in shared code
- **Cell boundaries:** When working across repository or monorepo boundaries

**When implicit cells are sufficient:**

- **Same-cell dependencies:** Most dependencies within the same cell
- **Local references:** Targets in the same cell you're currently working in

### Target Dependencies

Targets form a **Directed Acyclic Graph (DAG)**:

```
//app:main
├── //lib:utils
│   └── //third-party:json
└── //lib:core
    └── //third-party:json  (shared dependency)
```

**Key properties:**

- **Acyclic**: No circular dependencies allowed
- **Declared**: Dependencies must be explicitly listed in `deps`
- **Typed**: Dependencies flow through providers (see Providers section)

---

## Artifacts

Artifacts represent files in Buck2's build model.

### Types of Artifacts

#### 1. Source Artifacts

- **Definition:** Files checked into version control
- **Location:** Your repo (e.g., `src/main.cpp`)
- **Characteristics:**
  - Immutable (within a build)
  - No action produces them
  - Directly referenced in BUCK files

#### 2. Build Artifacts

- **Definition:** Files produced by actions during the build
- **Location:** `buck-out/` directory
- **Characteristics:**
  - Created by actions
  - Cached based on inputs
  - "Remember" what action produces them

**Example:**

```starlark
def my_rule_impl(ctx: AnalysisContext):
    # Source artifact (from repo)
    src = ctx.attrs.src

    # Build artifact (will be produced)
    output = ctx.actions.declare_output("result.o")

    # output "remembers" it will be produced by this action:
    ctx.actions.run(cmd_args(["gcc", src, "-o", output.as_output()]))
```

### Artifact Paths

Buck2 tracks artifacts symbolically during analysis:

```starlark
output = ctx.actions.declare_output("foo.txt")
# At this point, output is just a promise - file doesn't exist yet!
# Buck2 knows: "If anyone needs foo.txt, run the action that produces it"
```

### Bound vs Unbound Artifacts

- **Bound artifact:** Has an action that produces it
- **Unbound artifact:** Declared but no action produces it (Buck2 error!)

```starlark
# This will error:
def bad_rule_impl(ctx: AnalysisContext):
    output = ctx.actions.declare_output("result.txt")  # Declared
    return [DefaultInfo(default_output = output)]      # But never bound to an action!
```

---

## Actions

Actions are the commands that actually run during the build.

### Action Properties

1. **Hermetic**: Given the same inputs, always produce the same outputs
2. **Cacheable**: Results are cached based on inputs
3. **Lazy**: Only run when their outputs are needed
4. **Declared, not executed**: Rule implementations don't run actions - they
   declare them

### Creating Actions

#### ctx.actions.run()

Execute a command:

```starlark
ctx.actions.run(
    cmd_args(["my_tool", input_file, "-o", output.as_output()]),
    category = "process",        # For Buck2 UI
    env = {"VAR": "value"},      # Environment variables
)
```

#### ctx.actions.write()

Write content to a file:

```starlark
config_file = ctx.actions.write("config.json", '{"key": "value"}')
# Returns an artifact that can be used as input to other actions
```

#### ctx.actions.copy_file()

Copy a file:

```starlark
copied = ctx.actions.copy_file("output.txt", src_artifact)
```

### Action Inputs and Outputs

Buck2 tracks dependencies automatically:

```starlark
def my_rule_impl(ctx: AnalysisContext):
    src = ctx.attrs.src               # Input artifact
    output = ctx.actions.declare_output("result.txt")

    cmd = cmd_args(["process", src, "-o", output.as_output()])
    #                          ^^^              ^^^
    #                       input              output

    ctx.actions.run(cmd)
    # Buck2 now knows:
    # - This action reads: src
    # - This action writes: output
    # - This action must run after any action that produces src
```

### Action Caching

Buck2 caches action results based on:

- Command line arguments
- Input file contents (hash)
- Environment variables
- Execution configuration

If inputs haven't changed, Buck2 reuses cached outputs.

---

## Providers

Providers are how rules expose information to their dependents.

### Why Providers?

Without providers, dependents can't access information from their dependencies:

```starlark
# How does a binary know what headers its library dependencies export?
# How does a test know what binary to run?
# Answer: Providers!
```

### Built-in Providers

#### DefaultInfo

Every rule must return this:

```starlark
DefaultInfo(
    default_outputs = [artifact],    # List of primary outputs
    sub_targets = {"foo": [...]}     # Named sub-outputs
)
```

**Used by `buck2 build`:** When you run `buck2 build //target:name`, Buck2
builds the artifacts listed in `default_outputs`. This is what determines which
files get built.

**Example:**

```bash
# Builds the artifacts in DefaultInfo.default_outputs
buck2 build //app:main

# Access sub-targets
buck2 build //app:main[foo]  # Builds artifacts from sub_targets["foo"]
```

#### RunInfo

For executable targets:

```starlark
RunInfo(
    args = cmd_args([binary, "--flag"])  # How to run this target
)
```

**Used by `buck2 run`:** When you run `buck2 run //target:name`, Buck2 executes
the command specified in `RunInfo.args`. The target must provide `RunInfo` to be
runnable.

**Example:**

```bash
# Runs the command from RunInfo.args
buck2 run //app:main -- additional_args

# Buck2 will:
# 1. Build the target (using DefaultInfo)
# 2. Execute the command from RunInfo with any additional arguments
```

### Custom Providers

Define your own to pass custom information:

```starlark
# Define the provider
MyLibraryInfo = provider(fields = {
    "headers": provider_field(typing.Any),    # Header files
    "link_flags": provider_field(typing.Any), # Linker flags
})

# Library rule returns it
def my_library_impl(ctx: AnalysisContext):
    # ... build library ...
    return [
        DefaultInfo(default_output = lib_artifact),
        MyLibraryInfo(
            headers = ctx.attrs.headers,
            link_flags = ["-lmylib"],
        ),
    ]

# Binary rule consumes it
def my_binary_impl(ctx: AnalysisContext):
    all_headers = []
    all_link_flags = []

    for dep in ctx.attrs.deps:
        # Get the provider from dependency
        if MyLibraryInfo in dep:
            lib_info = dep[MyLibraryInfo]
            all_headers.extend(lib_info.headers)
            all_link_flags.extend(lib_info.link_flags)

    # Use headers and flags in compilation...
```

### Provider Propagation

Providers flow through the dependency graph **along declared dependency edges**.
A target can only access providers from its dependencies (targets listed in its
`deps` attribute).

**Example dependency chain:**

```starlark
# BUCK file for //app:main
my_binary(
    name = "main",
    deps = ["//lib:utils"],  # main depends on utils
)

# BUCK file for //lib:utils
my_library(
    name = "utils",
    deps = ["//third-party:json"],  # utils depends on json
)

# BUCK file for //third-party:json
my_library(
    name = "json",
)
```

**Dependency and provider flow:**

```
//app:main (needs headers)
    │ deps = ["//lib:utils"] (dependency relationship ↓)
    │ MyLibraryInfo provider flows ↑
//lib:utils (provides headers, also depends on json)
    │ deps = ["//third-party:json"] (dependency relationship ↓)
    │ MyLibraryInfo provider flows ↑
//third-party:json (provides headers)
```

**Important:** Dependencies flow downward (from dependent to dependency), but
providers flow **upward** (from dependency to dependent). When `//app:main`
depends on `//lib:utils`, the provider information flows from `//lib:utils` up
to `//app:main`.

**Key points:**

- **Dependencies must be declared:** `//app:main` can access providers from
  `//lib:utils` only because `//lib:utils` is in its `deps`
- **Transitive access:** The binary can collect headers transitively from all
  dependencies if the providers are propagated correctly
- **No dependency = no provider access:** Without a dependency edge, providers
  cannot flow between targets

---

## Configurations

Configurations determine platform-specific behavior.

### What is a Configuration?

A configuration is a **set of constraint values** that determine how a target is
built. Conceptually, it represents information like:

```
{os = linux, cpu = x86_64, compiler = gcc-11, opt_level = opt, ...}
```

This is not actual Buck2 syntax - it's a conceptual representation.

### Configuration Platform Files

Defined in Starlark:

```starlark
# fbcode//platform/linux.bzl
platform(
    name = "linux-x86_64",
    constraint_values = [
        "ovr_config//os:linux",
        "ovr_config//cpu:x86_64",
    ],
)
```

### How Configurations Are Applied

1. User specifies a target: `buck2 build //app:main`
2. Buck2 applies default configuration (or user-specified one)
3. Configuration resolves `select()` expressions
4. Different platforms → different configured targets

### select() Resolution

```starlark
cpp_binary(
    name = "app",
    srcs = ["main.cpp"],
    compiler_flags = select({
        "ovr_config//os:linux": ["-DLINUX"],
        "ovr_config//os:macos": ["-DMACOS"],
        "DEFAULT": [],
    }),
)

# On Linux: compiler_flags = ["-DLINUX"]
# On macOS: compiler_flags = ["-DMACOS"]
# On other platforms: compiler_flags = []
```

### Multi-Platform Builds

The same unconfigured target can be built for multiple platforms simultaneously:

```bash
buck2 build //app:main --target-platforms fbcode//platform/linux:x86_64
buck2 build //app:main --target-platforms fbcode//platform/macos:arm64
```

These create two distinct configured targets in the build graph.

---

## Analysis Phase

The analysis phase is when rule implementations run.

### What Happens During Analysis

1. Buck2 walks the configured build graph
2. For each configured target, calls the rule's `impl` function
3. Rule declares actions and returns providers
4. No actions are executed - just registered
5. Result: Complete action graph ready for execution

### Analysis Context

The `ctx` parameter provides:

```starlark
def my_rule_impl(ctx: AnalysisContext):
    # Access attributes
    srcs = ctx.attrs.srcs
    deps = ctx.attrs.deps

    # Access actions API
    output = ctx.actions.declare_output("result.txt")
    ctx.actions.run(...)

    # Access configuration
    is_windows = ctx.attrs._target_os_type.is_windows

    # Access dependencies' providers
    for dep in deps:
        info = dep[MyProvider]
```

### Analysis vs Execution

| Phase         | What Happens                              | When                       |
| ------------- | ----------------------------------------- | -------------------------- |
| **Analysis**  | Rule impl functions run, actions declared | When target is analyzed    |
| **Execution** | Actions run, files created                | When outputs are requested |

**Example:**

```starlark
def my_rule_impl(ctx):
    print("Analysis!")  # Prints during buck2 build (analysis)

    ctx.actions.run(
        cmd_args(["bash", "-c", "echo Execution!"]),  # Runs later (execution)
        ...
    )
```

---

## Build Graph

The build graph is a DAG of configured targets and actions.

### Graph Structure

```
Configured Targets → Actions → Artifacts

//app:main (linux)
    ↓ analysis produces
    [Action: link]
        ↓ depends on
    app.o (artifact)
        ↓ produced by
    [Action: compile main.cpp]
```

### Graph Queries

Buck2 provides tools to inspect the graph:

#### uquery - Unconfigured target graph

```bash
buck2 uquery "deps(//app:main)"        # All dependencies
buck2 uquery "rdeps(//..., //lib:foo)" # Reverse dependencies
```

#### cquery - Configured target graph

```bash
buck2 cquery "deps(//app:main)"                    # With configs applied
buck2 cquery //app:main --output-attribute=srcs   # Show resolved attributes
```

#### aquery - Action graph

```bash
buck2 aquery "deps(//app:main)"  # Show actions that will run
```

### Incremental Builds

Buck2 uses the graph for incrementality:

1. User changes `src/util.cpp`
2. Buck2 identifies affected artifacts (util.o)
3. Identifies actions that depend on util.o (link action)
4. Identifies configured targets that depend on those actions (//app:main)
5. Only re-runs necessary actions

**Result:** Fast incremental builds even in huge repositories.

---

## Summary

- **Targets**: Nodes in the build graph (unconfigured → configured)
- **Artifacts**: Files (source or build)
- **Actions**: Commands that produce artifacts
- **Providers**: Data flow between targets
- **Configurations**: Platform-specific settings
- **Analysis**: When rules declare actions
- **Build Graph**: DAG of targets, actions, and artifacts enabling incremental
  builds

These concepts work together to make Buck2 fast, correct, and scalable.
