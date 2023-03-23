---
id: configurations
title: Configurations
---

This page mostly focuses on how configurations and related features are implemented.

## Context

Buck configurations provide an API to express the different ways in which projects and targets can be built.

A configuration consists of a set of constraints and config settings (values from buckconfig). These are determined by a base platform that sets the initial values and then a series of transitions that may change them.

The common way that users are exposed to configurations is in `select()` invocations where the resolution is based on the configuration.

A build may involve many configurations. A particular target label (`//:foo`) may end up with multiple instances in the configured graph with different configurations.

## Selectable attributes

Almost all rule attributes can be set to a `select()` value; such an attribute is 'selectable'. These attributes' final resolved values will depend on the configuration.

There are some attributes that cannot use a `select()`; such attributes are termed 'not selectable'. Examples include attributes that buck needs to read from the unconfigured node (such as `name` and `default_target_platform`) and attributes that are used by `platform()` rules and their dependencies (see below).

## Selectable resolution

Resolving selectable attributes is pretty straightforward, it happens when constructing the 'configured target node'. At that point, the full configuration is available so Buck can lookup whether each constraint in the select is satisfied or not.

If multiple conditions of the select() match, then the select will be resolved to the 'most refined' of the conditions that match. A set of constraints (as in
a `config_setting`) is said to 'refine' another if it is a superset of that other's constraints. The 'most refined' of a set is then the condition that refines all the others. If there is no 'most refined' condition of the matching ones, it is an error.

## Target Platform Resolution

In the event that targets are provided on the command line, or when there is no indication of what configuration the target will be built in, configurations are determined by performing 'target platform resolution' on the unconfigured target labels.

The target platform resolution for a target `//:foo` works as follows:

1. Look up (unconfigured) target node for `//:foo`.
1. If the command has a `--target-platforms` flag, use that.
1. If there's a `default_target_platform` attribute, use that.
1. Else, use the cell's default platform.

This is performed independently for any targets that need a platform. Since this resolution is done without a configuration, it means that the `default_target_platform` attribute **is not selectable**.

This target platform will form the initial configuration for the node.

## Configuration propagation

Once the top-level nodes have been configured via the target platform resolution, the configuration is propagated to dependencies (possibly altered by transitions).

:::note
The target platform resolution is not applied to all nodes in the graph.
:::

## Transitions

A transition transforms a configuration by adding or changing constraint values and config settings or by setting an entirely new underlying target platform.

For more details, see [Configuration transitions](configuration_transitions.md).

## `ConfigurationInfo`, `platform()` analysis, and more

The definition of a platform (either execution or target) is done with a `platform` rule instance. The configuration is actually part of the analysis result of the platform target (the `ConfigurationInfo` provider instance). This is convenient from
an implementation standpoint, but it leads to a situation where some nodes are analyzed with an 'unbound' Configuration.

All the rule types involved in defining a platform may be analyzed with an unbound configuration (`platform()`, `config_setting()`, `constraint_setting()`, and so on). These are sometimes called 'configuration rules'. This means that all the attributes of these rules are not selectable.

Configurations also reference a few other provider instances such as `ConstraintSettingInfo`. All of these end up being potentially produced in a context with an unbound configuration.

Using analysis for this also means that 'configuration' and 'analysis' are not distinct phases within a build (although they are still distinct for a node and are still conceptually useful).

## Configurations and output paths

Since a target may appear within a build in multiple different configurations, output paths cannot be derived based on just targets (as multiple actions would map to the same outputs). For this reason, the target and the configuration are encoded into output paths. The configuration is currently represented as a hash of its values (a 'hashed buck-out').

## Target platform compatibility

All (non-configuration) rules support a `target_compatible_with` attribute. In addition, the rule itself can define `target_compatible_with` constraints that affect all instances. The `target_compatible_with` attribute is a list of constraints/config settings and it **is selectable**.

Target platform compatibility is transitive, all *dependents* of an incompatible target are incompatible. In other words, a node is compatible if and only if the node itself and all of its transitive dependencies are compatible.

In buck, this is implemented by graph configuration returning either a configured target node or an indicator that the node is incompatible with the target platform.

### Buck v1 compatibility

Buck2 also supports the Buck v1 legacy `compatible_with` field on nodes but it has different behavior.

In summary:

* `compatible_with`: List of constraints, where *any* of them must match the configuration to be compatible.
* `target_compatible_with`: List of constraints, where *all* of them must match the configuration to be compatible.

## Incompatible target skipping

In a build-like command where a non-literal target pattern is provided (for example, `buck build //:` or `buck build //foo/...`), the target pattern will be resolved to a set of unconfigured targets. Those targets will then go through [target platform resolution](#target-platform-resolution). If any of those targets resolve to a platform where they are incompatible, building them will be skipped. Users generally expect and prefer this behavior to needing to explicitly specify only the targets that can build in their current context.

If an explicitly specified literal is incompatible, it is an error.

The implementation checks compatibility when looking up the analysis results for configured nodes requested (in the non-ignored flow, it uses
that analysis result to lookup the default outputs and build them).

## Execution platforms

Execution platforms/configurations are used to represent the platforms where build execution happens. These are defined in a similar manner to target platforms.
These may or may not be what one would logically consider different 'platforms'. For example, there could be multiple different execution platforms that all execute things similarly on the local machine.

A build configures a fixed list of one or more execution platforms.

## Execution deps

Some target deps are 'execution deps'. These are the dependencies of the target that should be built for the execution platform. For example, a compiler or other build tool would be an execution dep. This includes all exe macro deps (for example, `$(exe //:tool)`) and includes all `attrs.exec_dep()` deps.

## Toolchain deps

In addition to `attrs.exec_dep()`, there are `attrs.toolchain_dep()`, which are similar but differ in an important way. These nodes don't select their execution platform, but instead have it forced on them by whatever includes them; hence, it must be recorded in the configured target label. The execution platform resolution sees through them.

In other words, `attrs.toolchain_dep()` is like a mix of `attrs.dep()` and `attrs.exec_dep()`: it inherits target platform like `attrs.dep()` (so any
`select()`s on the target of the `attrs.toolchain_dep()` will evaluate as if they were on the target containing the `attrs.toolchain_dep()` - the target
platform gets inherited as normal) and any `attrs.exec_dep()`s of the `attrs.toolchain_dep()` target become `attrs.exec_deps()` on the dependent of
target the `attrs.toolchain_dep()` (they get passed up the dep tree, so participate in exec platform resolution).

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

The above means that `:C` will be an execution dependency of `:A` and any `select()`s defined in `:B` would be evaluated against the same target platform as `:A` (as target platform gets inherited by `attrs.toolchain_dep()`s).

## Running non-execution deps

If you have a binary that you want to run, but it isn't a build tool, then you should use `$(exe_target //:binary)` rather than `$(exe //:binary)`. That will run the same binary that you'd get from `buck2 build`, rather than one that is built for the execution platform.

## Execution platform resolution

During analysis, unlike target platform resolution, every configured node undergoes execution platform resolution independently (see exception below). This
means that even for a specific target platform, different nodes in the graph can be built on different execution platforms.

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

One important note here is that until the execution platform has been resolved, **the configuration for execution deps is not known**. Only after
execution platform has been resolved can the execution deps be configured (also, analysis for them can only be performed at that point).

For the normal use case, a particular configured target node performs execution platform resolution a single time. The execution platform **is not** encoded in output paths.

Regarding target compatibility, imagine the following pseudo-code for the `target_compatible_with()` function above:

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

## Execution groups

Execution groups are a future feature that will allow a rule to perform execution platform resolution multiple times and then specify in which of the resolved
platforms each action runs in.
