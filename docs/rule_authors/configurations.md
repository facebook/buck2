# Configurations

This doc focuses mostly on how configurations and related features are implemented. For
the user docs (which are also quite helpful for understanding this), see ????? (for now
try the [buck1 docs](https://buck.build/) or the [bazel docs](https://docs.bazel.build/versions/master/skylark/config.html) for similar features that buck2 has been
modeled after).

TODO(cjhopman): Configurations needs some user docs. Could probably skip some of the below context if we had them.

## Context

Buck "Configurations" provides an API to express the different ways projects and targets can be built.

A configuration consists of a set of constraints and config settings (i.e. values from buckconfig). These are determined by a base platform
that sets the initial values and then a series of transitions that may change them.

The common way that users are exposed to configurations is in `select()` invocations where the resolution is based on the
configuration.

A build may involve many configurations and even a particular target label (`//:foo`) may end up with multiple
instance in the configured graph with different configurations.

### `platform()`, `constraint_setting()`, `config_setting()`, etc

TODO(cjhopman): Describe this enough for context needed below

## Selectable attributes

Almost all rule attributes can be set to a `select()` value, such an attribute is "selectable". These attributes final resolved value will depend on the configuration.

There are some attributes that cannot use a `select()`, such attributes are "not selectable". Examples include attributes that buck needs to
read from the unconfigured node (ex. "name", "default_target_platform") and attributes that are used by `platform()` rules and their
dependencies (see below).

TODO(cjhopman): Should rule authors be able to set attrs as not selectable?

## Selectable resolution

Resolving selectable attributes is pretty straightforward, it happens when constructing the "configured target node". At that point, we have the full configuration
available and se we can lookup whether each constraint in the select is satisfied or not.

TODO(cjhopman): Describe how checking refinement is implemented.

## Target Platform Resolution

When targets are provided on the command line (e.g. `buck build //:foo`) or otherwise where there's no indication of what configuration
that target will be built in, configurations are determined by performing "target platform resolution" on
the unconfigured target labels.

Target Platform Resolution for a target `foo` works by:

1. lookup (unconfigured) target node for "foo"
2. if there's a "default_target_platform" attribute, use that
3. else, use the cell's default platform

This is performed indepedently for any targets that need a platform. Since this resolution is done without
a configuration, it means that the default_target_platform attribute **is not selectable**.

This target platform will form the initial configuration for the node.

TODO(cjhopman): how does a user explicitly specify on the command line a target platform such that the target does not
go through target platform resolution? Are there other cli options/flags/etc that affect target platform resolution?

## Configuration propagation

Once we've configured the top-level nodes via target platform resolution, the configuration is propagated to dependencies (possibly altered by transitions).

**We do not apply target platform resolution to all nodes in the graph.**

## Transitions

A "transition" transforms a configuration by adding or changing constraint values and config settings or by setting an entirely new underlying target platform.

More details in: [Configuration transitions](configuration_transitions.md).

## ConfigurationInfo, `platform()` analysis, and more

The definition of a platform (either execution or target) is done with a "platform" rule instance. The configuration is actually
part of the analysis result of the platform target (the ConfigurationInfo provider instance). This is convenient from
an implementation standpoint, but it leads to a situation where some nodes are analyzed with an "unbound" Configuration. All the
rule types involved in defining a platform may be analyzed with an unbound configuration (`platform()`, `config_setting()`,
`constraint_setting()`, etc). These are sometimes called "configuration rules". This also means that all the attributes of these rules are not selectable.

Configurations also reference a few other provider instances like ConstraintSettingInfo. All of these end up being potentially
produced in a context with an "unbound" configuration.

Using analysis for this also means that "configuration" and "analysis" are not distinct phases within a build (though are
still distinct for a node and are still conceptually useful).

## Configurations and output paths

Because a target may appear within a build in multiple different configurations we cannot derive output paths based on
just targets (as then we'd have multiple actions map to the same outputs). For this reason, we encode the target and the configuration
into output paths. The configuration is currently represented as a hash of its values (i.e. "hashed buck-out").

## Target platform compatibility

All (non-configuration) rules support a `target_compatible_with` attribute. In addition, the rule itself can define
`target_compatible_with` constraints that affect all instances. The `target_compatible_with` attribute is a list of
constraints/config settings and it **is selectable**.

Target platform compatibility is transitive, all *dependents* of an incompatible target are incompatible. In other words,
a node is compatible if and only if the node itself and all of its transitive dependencies are compatible.

In buck, this is implemented by target analysis returning either the normal analysis result or an indicator
that the node is incompatible with the target platform. It is not part of the "configured target node" because
the "configured target node" does not depend on information from dependencies.

TODO(cjhopman): Something about debuggability of incompatibility, especially due to needing it to figure out transitive incompatibility.

### Buck v1 compatibility

Buck2 also supports the Buck v1 legacy `compatible_with` field on nodes but it has different behavior. In summary:

- `compatible_with`: List of constraints, *any* of them must match the configuration to be compatible.
- `target_compatible_with`: List of constraints, *all* of them must match the configuration to be compatible.

## Incompatible target skipping

In a build-like command where a non-literal target pattern is provided (ex. `buck build //:` or `buck build //foo/...`) the target pattern
will be resolved to a set of unconfigured targets. Those targets will then go through "target platform resolution". If any of those targets
resolve to a platform where they are incompatible, building them will be skipped. Users generally expect and prefer this behavior to
needing to explicit specify only the targets that can build in their current context.

If an explicitly specified literal is incompatible, it is an error.

The implementation checks compatibility when looking up the analysis results for configured nodes requested (in the non-ignored flow, it uses
that analysis result to lookup the default outputs and build them).

## Execution platforms

Execution platforms/configurations are used to represent the platforms where build execution happens. These are defined similar to target platforms.
These may or may not be what one would logically consider different "platforms", for example, there could be multiple different execution platforms
that all execute things similarly on the local machine.

A build configures a fixed list of one or more execution platforms.

## Execution deps

Some target deps are "execution deps". These are the dependencies of the target that should be built for the execution platform. For example,
a compiler or other build tool would be an execution dep. This includes all exe macro deps (ex. `$(exe //:tool)`) and includes all `attrs.exec_dep()` deps.

## Toolchain deps

In addition to `attrs.exec_dep()`, we have an `attrs.toolchain_dep()` primitive that's similar but different in an important way. These nodes don't select
their execution platform, but instead have it forced on them by whatever includes them, hence it must be recorded in the configured target label.
The execution platform resolution sees through them.

In other words, `attrs.toolchain_dep()` is like a mix of `attrs.dep()` and `attrs.exec_dep()`: it inherits target platform like `attrs.dep()` (so any
`select()`s on the target of the `attrs.toolchain_dep()` will evaluate as if they were on the target containing the `attrs.toolchain_dep()` - i.e., target
platform gets inherited as normal) and any `attrs.exec_dep()`s of the `attrs.toolchain_dep()` target become `attrs.exec_deps()` on the dependent of
target the `attrs.toolchain_dep()` (i.e., they get passed up the dep tree, so participate in exec platfom resolution).

Illustrated as an example:

```
target(
    name = "A",
    toolchain = attrs.toolchain_dep(default = ":B"),
)
target(
    name = "B",
    tool = attrs.exec_dep(default = ":C")
)
```

The above means that `:C` will be an execution dependency of `:A` and any `select()`s defined in `:B` would be evaluated against
the same target platform as `:A` (as target platform gets inherited by `attrs.toolchain_dep()`s).


## Running non-execution deps

If you have a binary that you want to run, but it isn't a build tool, then you should use `$(exe_target //:binary)` rather than `$(exe //:binary)`. That
will run the same binary that you'd get from `buck2 build`, rather than one that is built for the execution platform.

## Execution platform resolution

During analysis, unlike target platform resolution, every configured node undergoes execution platform resolution independently (see exception below). This
means that even for a specific target platform, different nodes in the graph can be built on different execution platforms.

This works roughly like this:

```python
next: for platform in execution_platforms:
    if exec_compatible_with(target, platform):
        for dep in target.execution_deps():
            if !target_compatible_with(dep, platform):
              continue next
        return platform
return err
```

One important note here is that until the execution platform has been resolved, **we do not know the configuration for execution deps**. Only after
we've resolved the execution platform can we configure the execution deps (and can also only perform analysis for them at that point).

For the normal case, a particular configured target node performs execution platform resolution a single time. The execution platform
**is not** encoded in output paths.

Regarding target compatibility, you can imagine the following pseudo-code for the `target_compatible_with()` function above:

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

TODO(cjhopman): What are the requirements on rule authors? Especially, do they need to ensure that all possible execution platforms will produce the same results?

## Execution groups

Execution groups allow a rule to do execution platform resolution multiple times and then specify which of the resolved platforms each
action runs in.

TODO(cjhopman): Finish this documentation as we figure it out.

## Execution platform inheritance

There are some (rare) cases where both a target and its dependency need to resolve to the same execution platform.

An example of this would be a c++ toolchain that includes both target (ex. the stdlib) and
execution (ex. the compiler) components. In that case, we need the toolchain's execution deps to affect its users
execution platform resolution.

To support this, there are mechanisms to indicate which dependencies need to inherit the execution platform resolution.

Currently, since this could mean that a configured node appears in the build under multiple different execution platforms and since execution
platforms are not included as part of output paths, users must use this feature carefully.

TODO(cjhopman): figure out what restrictions this needs to apply so that users don't need to be the ones responsible for correctness
TODO(cjhopman): figure out/document those "mechanisms"
