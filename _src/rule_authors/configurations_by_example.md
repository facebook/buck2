---
id: configurations_by_example
title: Configurations By Example
---

[Buck’s architectural model](../../developers/architecture/buck2/) description
is a very helpful pre-read.

The main use of configurations is changing target properties based on what the
build is targeting, which may include platform properties like OS, architecture,
runtime version (think java, python) etc and other build properties like
optimization level

An example of how that’s done:

```python
# //libs/BUCK

java_library(
name = "foo",
deps = [
    "//libs:lib1",
    "//libs:lib2",
] + select({
    "//constraints:x86": ["//libs:lib3-x86"],
    "//constraints:mac-arm64": ["//libs:lib3-mac-arm64"],
    "//constraints:windows-arm64": ["//libs:lib3-win-arm64"],
    "DEFAULT": ["//libs:lib3-general"],
})
...
)
...
```

- select() can appear in almost all attributes
  - since example above has lists of a single element, it could’ve been a select
    for a single element in the list rather than added to the list. that’s
    pretty inflexible (can’t have empty cases, each case must be exactly one
    element) and so it wouldn’t generally be used
- string, list, dict can all be added to select (on either side): list + select,
  select + list, str + select, …
- Each branch of select() takes a config_setting (described below), which
  denotes a list of required constraint_values; there’s also an optional
  ”DEFAULT” branch to the select. The target platform resolution rules (below)
  pick a platform, which itself gives a list of provided constraint_values. A
  branch matches if all its required constraint_values are provided by the
  platform. If no branch matches then the DEFAULT branch is used (or failure if
  there’s no DEFAULT branch); if one branch matches it is used, if more than one
  branch matches then see the “select resolution ambiguity (refinement)” section
  below.
- select() is resolved during configuration. this happens after the evaluation
  of the BUCK file is completed, and so starlark code run during BUCK file
  evaluation does not have access to the resolved value. This can make it
  difficult to have macros that do extensive modification or inspection of
  attributes (and certainly we encourage doing that in rules instead). There are
  some functions to do some limited operations on these objects:
  - select_map(obj, function): applies function to all possible resolved values
    in obj
    - ex:
      `select_map([1] + select({x: 2, y: 3}), lambda v: v+1) == [2] + select(x: 3, y: 4)`
  - select_test(obj, function): function should return a bool, then applies
    function to each resolved value and returns True if function returns True
    for any of them

## Defining Configurations

First, define constraints and config settings. Defining constraints is done with
constraint_setting and constraint_value. constraint_setting in some sense is the
ID of a group of constraints each defined with constraint_value. In any
configuration, only one value can be present for a constraint_setting. The
config_setting rule allows creating a logical AND of constraints, and also can
require that buckconfig keys have certain values.

```python
# //constraints/BUCK

# constraint_setting defines a key for a logical group of constraint values. A configuration can only
# have at most one constraint value set for each constraint_settings
constraint_setting(
    name = "arch",
)

constraint_value(
    name = "x86",
    constraint_setting = ":arch",
)

constraint_value(
    name = "arm64",
    constraint_setting = ":arch",
)

constraint_setting(
    name = "os",
)

constraint_value(
    name = "windows",
    constraint_setting = ":os",
)

constraint_value(
    name = "mac",
    constraint_setting = ":os",
)

constraint_setting(
    name = "mode",
)

constraint_value(
    name = "dev",
    constraint_setting = ":mode",
)

constraint_value(
    name = "opt",
    constraint_setting = ":mode",
)

# can use config_setting to group constraint values into larger logical pieces
config_setting(
    name = "mac-arm64",
    constraint_values = [
        ":mac",
        ":arm64",
    ]
)

config_setting(
    name = "windows-arm64",
    constraint_values = [
        ":windows",
        ":arm64",
    ]
)

# an example of checking a buckconfig value. If the buckconfig is set,
# this config_setting is satisfied in all configurations
config_setting(
    name = "check_some_config",
    values = {
        "foo.fastmode_enabled": "true",
    }
)
```

Next, define platforms (which, confusingly, create what we call a
configuration). platforms are just a collection of constraints. A platform() can
have other platforms as deps and will union the constraints associated with that
platform. this example shows a couple techniques that can be helpful for
defining platforms

```python
#//platforms/BUCK

[
    platform(
        name = "{}-{}".format(base, mode)
        deps = [":{}".format(base)],
        constraint_values = ["//constraints:{}".format(mode)]
    )
    for base in ["mac-x86", "mac-arm64", "windows-x86", "windows-arm64"]
    for mode in ["dev", "opt"]
]

[
    platform(
        name = name,
        constraint_values = constraint_values
    ) for name, constraint_values in [
        ("mac-x86", ["//constraints:mac", "//constraints:x86"]),
        ("mac-arm64", ["//constraints:mac", "//constraints:arm64"]),
        ("windows-x86", ["//constraints:windows", "//constraints:x86"]),
        ("windows-arm64", ["//constraints:windows", "//constraints:arm64"]),
    ]
]
```

## Target Platform Resolution

The one remaining piece to put these all together is about selecting a target
platform for the top-level targets.

In the case that targets are provided on the command line, configurations are
determined by performing 'target platform resolution' on the unconfigured target
labels.

The target platform resolution for a target //:foo works as follows:

1. Look up (unconfigured) target node for //:foo.
1. If the command has a --target-platforms flag, use that.
1. If there's a default_target_platform attribute on the node, use that.
1. Else, use the cell's default platform spec (from buckconfig
   parser.target_platform_detector_spec).

This is performed independently for any top-level targets that need a platform.
Since this resolution is done without a configuration, it means that the
default_target_platform attribute is not selectable.

This target platform will form the initial configuration for the node and will
be passed down to all of the target dependencies of that node (exceptions, like
exec deps, are described below).

Example:

```python
# //binaries/BUCK

java_binary(
    name = "cats",
    default_target_platform = "//platforms:windows-arm64-dev",
    deps = ["//libs:foo"],
)

java_binary(
    name = "dogs",
    default_target_platform = "//platforms:mac-x86-dev",
    deps = ["//libs:foo"],
)
```

If you then do `buck2 build //binaries:cats //binaries:dogs`, the
//binaries:cats binary will be built in the //platforms:windows-arm64-dev
configuration and the //binaries:dogs binary will be built in the
//platforms:mac-x86-dev configuration. Each of those binaries depend on
//libs:foo, but they will get different versions of it as the binaries’
configurations will each be passed down to their dependencies.

If you look at the //libs:foo defined above, for //binaries:cats its resolved
dependencies will include //libs:lib3-win-arm64 and for //binaries:dogs it would
contain //libs:lib3-x86.

You can specify a different target platform on the command line. If you run

`buck2 build //binaries:cats //binaries:dogs --target-platforms //platforms:mac-x86-opt`,
both //binaries:cats and //binaries:dogs will be built in the
//platforms:mac-x86-opt configuration.

## Target Compatibility

If a target doesn’t work when built targeting certain platforms or
configurations, it can specify this by setting target_compatible_with. This
attribute is a list of constraints that a configuration must have otherwise the
target will be marked as incompatible with that configuration.

```python
# //other/BUCK

default_target_platform = "//platforms:mac-x86-dev" if host_info().os == "mac" else "//platforms:win-x86-dev"

...

java_binary(
    name = "other",
    deps = [":other_lib"],
    default_target_platform = default_target_platform,
)

java_library(
    name = "other_lib",
    target_compatible_with = [
        "//constraints:dev",
        "//constraints:win",
    ]
)
```

Running `buck2 build //other:other --target-platforms //platforms:win-x86-dev`
would build other in that configuration. But running
`buck2 build //other:other --target-platforms //platforms:mac-x86-dev` would
fail, because //other:other_lib would be incompatible with that configuration
and so //other:other would be as well. buck considers it an error to request to
build (or run or install or test) an explicit target that is incompatible.

If a package (ex //other:) or recursive (ex //other/...) pattern is provided, it
is not an error for that to include incompatible targets and they will instead
simply be skipped (buck should print a message that it is skipping them). In
this example, the default_target_platform is being selected based on the host
(you could imagine this being commonly done within some small macro layer that
your project uses). There may be other targets in the //other/BUCK file that are
compatible with mac, and so if you do `buck2 build //other:` that could build
all the targets in that package that are compatible with their
default_target_platform and if they all used the same as //other:other some of
them may be compatible with mac when building on a mac and those would be built
fine (and //other:other would be skipped).

# Advanced topics

## Execution Platforms

Execution platforms are used to define the configurations and execution
properties for the platforms used by build tools during the build. Currently
there is a single list (in priority order) of all available execution platforms.
This list is provided by a target in the build.execution_platforms buckconfig
configuration key.

> To Buck, both execution platforms and the list of them are based on
> ExecutionPlatformInfo and ExecutionPlatformRegistrationInfo, but we’ll talk in
> terms of the execution_platform and execution_platforms rules.

There are three main concepts to understand about execution platforms:

1. execution platforms
2. exec deps
3. execution platform resolution

Here’s an example definition of execution platforms.

```python
# //platforms/execution/BUCK

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
    # in practice, may want to change this order based on the host os.
    platforms = [
        "linux-exec",
        "windows-exec",
        "mac-exec",
    ],
    fallback = "error",
)
```

This sets us up with three execution platforms, one for each of windows, mac,
and linux. We choose a more optimized configuration for that platform (i.e. opt
instead of dev). Generally for build tools we’d recommend using an optimized
form as most of the time the build will be executing the built tools rather than
building them.

## Exec Deps

Exec deps are the second part of the execution platform system. An exec dep
differs in two ways from a normal dep:

1. It will inherit the execution platform of its dependent instead of the target
   platform and
1. A dependent’s execution platform will be selected so that all exec deps are
   target compatible with it.

Exec deps should be used for build tools that will be used when executing the
actions of a target. If information about the dep is going to be propagated out
of the target it almost always should not be an execution dep (except for
toolchains, see below).

Exec deps are added primarily in two ways:

1. By rule attributes defined with attr.exec_dep() and
1. By $(exe xxx) placeholders in attributes defined with attr.arg()

```python
foo_rule = rule(
  impl = <...>
```

# Visualizing Configuration Concepts

## Graph with deps

![Example graph with dependencies](/img/configurations/graph_with_deps.png)

## Splitting //:lib3

As we work out the configurations here, //:lib3 will end up being in two
different configurations, so gonna be easiest to split it now.

## Execution Platform resolution

![Example graph with dependencies](/img/configurations/execution_platform_resolution.png)

This shows which nodes are involved in determining the exec configuration for
the //:binary target. The exec deps of //:binary and the exec deps for the
(transitive) toolchain deps of //:binary are the main things involved, that set
of exec deps must all be target compatible with an execution platform for it to
be selected. In addition, the target itself and its toolchain deps must be
exec_compatible_with. It is very rare to use exec_compatible_with, for the most
part exec platform restrictions should be marked on the tools that require the
restriction.

## Target configurations

![Example graph with dependencies](/img/configurations/graph_with_target_configurations.png)
