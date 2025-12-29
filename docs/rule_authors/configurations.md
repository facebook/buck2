---
id: configurations
title: Configurations
---

This page mostly focuses on how configurations and related features are
implemented. A good understanding of the high-level
[concepts](../concepts/configurations.md) is therefore required.

## Defining platforms

A platform is simply a target with at least a
[`PlatformInfo`](../api/build/PlatformInfo/). That target can be an
instance of a custom rule, or it can simply use the prelude's
[`platform`](../prelude/rules/core/platform/) rule:

```python
platform(
    name = "my_platform",
    constraint_values = [
        "//constraints:build_mode[debug]",
        "//constraints:cpu[x64]",
    ],
)
```

The configuration is actually part of the analysis result of the
platform target (the
[`ConfigurationInfo`](../api/build/ConfigurationInfo/) provider
instance). This is convenient from an implementation standpoint, but it
leads to a situation where some nodes are analyzed with an "unbound"
Configuration.

All the rule types involved in defining a platform may be analyzed with an
unbound configuration (`platform()`, `config_setting()`, `constraint_setting()`,
and so on). These are sometimes called "configuration rules". This means that
all the attributes of these rules are not selectable.

Configurations also reference a few other provider instances such as
`ConstraintSettingInfo`. All of these end up being potentially produced in a
context with an unbound configuration.

Using analysis for this also means that "configuration" and "analysis" are not
distinct phases within a build (although they are still distinct for a node and
are still conceptually useful).

## Execution Platforms

> To Buck, both execution platforms and the list of them are based on
> `ExecutionPlatformInfo` and `ExecutionPlatformRegistrationInfo`, but
> we’ll talk in
> terms of the `execution_platform` and `execution_platforms` rules.

There are three main concepts to understand about execution platforms:

1. Execution platforms
2. Execution deps
3. Execution platform resolution

Here’s an example definition of execution platforms.

```python
execution_platform(
    name = "mac-exec",
    platform = "//platforms:mac-arm64-opt",
    local_enabled = host_info().os.is_macos,
    remote_enabled = True,
    use_limited_hybrid = False,
    remote_execution_use_case = "buck2-build",
    remote_execution_properties = {
        "platform": "mac-re"
    },
)

execution_platform(
    name = "windows-exec",
    platform = "//platforms:windows-arm64-opt",
    local_enabled = host_info().os.is_windows,
    ...
)

execution_platform(
    name = "linux-exec",
    ...
)

execution_platforms(
    name = "exec-platforms",
    # In practice, may want to change this order based on the host os.
    platforms = [
        "linux-exec",
        "windows-exec",
        "mac-exec",
    ],
    fallback = "error",
)
```

This sets us up with three execution platforms, one for each of Windows,
macOS, and Linux. We choose a more optimized configuration for that
platform (i.e. `opt` instead of `dev`). Generally for build tools we
recommend using an optimized form, as most of the time the build will be
executing the prebuilt tools rather than building them.

## Execution deps

Some target deps are "execution deps". These are the dependencies of the
target that should be built for the execution platform. For example, a
compiler or other build tool would be an execution dep. This includes
all exe [string parameter macro](./string_parameter_macros.md) deps (for
example, `$(exe //:tool)`) and includes all `attrs.exec_dep()` deps.

An exec dep differs in two ways from a normal dep:

1. It will inherit the execution platform of its dependent instead of the target
   platform
1. A dependent's execution platform will be selected so that all exec deps are
   target compatible with it.

## Toolchain deps

In addition to `attrs.exec_dep()`, there are `attrs.toolchain_dep()`, which are
similar but differ in an important way. These nodes don't select their execution
platform, but instead inherit the execution platform of whatever target
references them; hence, it must be recorded in the configured target label. In
some sense, execution platform resolution sees through them.

In other words, `attrs.toolchain_dep()` is like a mix of `attrs.dep()` and
`attrs.exec_dep()`:

- It inherits its target platform from the dependent build target like
  `attrs.dep()` (so any `select()`s using the target of the
  `attrs.toolchain_dep()` will evaluate as if they were on the target
  referencing the `attrs.toolchain_dep()` - the target platform gets inherited
  as with `attrs.dep()`)
- Like `attrs.exec_dep()` itself, `attrs.exec_dep()`s of the
  `attrs.toolchain_dep()` target are inserted into the list of
  `attrs.exec_dep()` on the dependent target of the `attrs.toolchain_dep()`
  (they get passed up the dep tree, so participate in exec platform resolution).

This is illustrated in the following example:

```python
target(
    name = "A",
    toolchain = attrs.toolchain_dep(default = ":B"),
)
target(
    name = "B",
    tool = attrs.exec_dep(default = ":C")
)
```

The above means that `:C` will be an execution dependency of `:A` and any
`select()`s defined in `:B` would be evaluated against the same target platform
as `:A` (as target platform gets inherited by `attrs.toolchain_dep()`s).

## Execution platform resolution

During analysis, unlike target platform resolution, every configured node
undergoes execution platform resolution independently (see exception below).
This means that even for a specific target platform, different nodes in the
graph can be built on different execution platforms.

This works roughly as follows:

```python
next: for platform in execution_platforms:
    if exec_compatible_with(target, platform):
        for dep in target.execution_deps():
            if !target_compatible_with(dep, platform):
              continue next
        return platform
return err
```

One important note here is that until the execution platform has been resolved,
**the configuration for execution deps is not known**. Only after execution
platform has been resolved can the execution deps be configured (also, analysis
for them can only be performed at that point).

For the normal use case, a particular configured target node performs execution
platform resolution a single time. The execution platform **is not** encoded in
output paths.

Regarding target compatibility, imagine the following pseudo-code for the
`target_compatible_with()` function above:

```python
def target_compatible_with(target, cfg):
    for constraint in target.target_compatible_with:
        if not satisfied(constraint, cfg):
            return False

    if len(target.compatible_with) > 0:
        found_satisfied_constraint = False
        for constraint in target.compatible_with:
            if satisfied(constraint, cfg):
                found_satisfied_constraint = True
                break
        if not found_satisfied_constraint:
            return False

    for (dep, dep_cfg) in direct_deps(target):
        # NB: recursive call
        if not target_compatible_with(dep, dep_cfg):
            return False

    return True
```

## Visualizing configuration concepts

### Graph with deps

![Example graph with dependencies](/img/configurations/graph_with_deps.png)

### Splitting //:lib3

As we work out the configurations here, `//:lib3` will end up being in two
different configurations, so gonna be easiest to split it now.

### Execution Platform resolution

![Example graph with dependencies](/img/configurations/execution_platform_resolution.png)

This shows which nodes are involved in determining the exec configuration for
the `//:binary` target. The exec deps of `//:binary` and the exec deps for the
(transitive) toolchain deps of `//:binary` are the main things involved, that set
of exec deps must all be target compatible with an execution platform for it to
be selected. In addition, the target itself and its toolchain deps must be
`exec_compatible_with`. It is very rare to use `exec_compatible_with`, for the most
part exec platform restrictions should be marked on the tools that require the
restriction.

### Target configurations

![Example graph with dependencies](/img/configurations/graph_with_target_configurations.png)

## See also

- [Configuration transitions for rule
  authors](./configuration_transitions.md)
