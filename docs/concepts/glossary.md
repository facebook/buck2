---
id: glossary
title: Glossary
---

#### .buckconfig
The root of your [project](#project) must contain a configuration file named `.buckconfig`. Before executing, Buck2 reads this file to incorporate specified customizations. Performs the same role as it did in Buck1. See more: [Legacy docs](https://buck2.build/docs/legacy/files-and-directories/dot-buckconfig).

#### Action
An individual, cacheable, ideally hermetic command that's run during the [build](#build). It takes [artifacts](#artifact) as inputs and produces other artifacts as outputs. An example command could be `gcc -o main main.c` which takes the artifact `main.c` (a source file) and produces the artifact called `main` (the compiled binary).

#### Action graph
It's the dependency graph of all the [actions](#action) belonging to a target. Can be queried with `buck2 aquery`.

#### Artifact
A single input or output of an [action](#action). These are files that participate as inputs or outputs of a build. These can be source files or build outputs. See also: [Artifact API](https://buck2.build/docs/generated/native/Artifact/).

#### Attribute

Declared by a [rule](#rule), used to express the properties of a particular instance of a rule to create a [target](#target). For example srcs, deps and copts, which declare a target's source files, dependencies, and custom compiler options, respectively. The available attributes for a target depend on it's rule type.

#### BUILD file

A BUILD file (also often named `BUCK` or `TARGETS`, this is configurable) is the main configuration file that tells Buck2 what to build, what their dependencies are and how to build them. Buck2 takes a BUILD file as input and evaluates the file to declare [targets][#target], which are then used to create a graph of dependencies and to derive the [actions](#action) that must be completed to build intermediate and final software outputs. A BUILD file marks a directory and any sub-directories not containing a BUILD file as a [package](#package).


#### Bxl

[Buck Extension Language](https://buck2.build/docs/developers/bxl). BXL scripts are written in [Starlark](https://github.com/bazelbuild/starlark), a restricted subset of Python, and give integrators the ability to inspect and interact directly with the buck2 graph. BXL scripts can query the [action graph](#action-graph), [configured graph](#configured-graph), and [unconfigured graph](#unconfigured-graph), create [actions](#actions) and trigger builds.

#### Cell
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Configuration
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Configured graph
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Constraint value
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Daemon
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Dependency

A directed edge between two [targets](#target). A target `A` can have a dependency on target `B`, for example, if any `dep` attribute of `A` mentions `B`. Whether a target can depend on another target depends on the [visibility](#visibility) of the later.

#### Execution platform

A type of [rule](#rule) that includes information such as which execution types a [target](#target) supports, which can be [remote](#remote-execution-re), local and [hybrid](#hybrid-execution) execution. Also whether it supports cache uploads, which allows us to get cache hits for things that executed locally.

#### Hybrid execution

Enables shifting work to the local host when available parallelism in the build is low. This lets us save on [remote execution](#remote-execution-re) round-trips to enable faster builds.

#### Isolation dir

Instances of Buck2 share a [daemon](#daemon) if and only if their isolation directory is identical. The isolation directory also influences the output paths provided by Buck2.

#### Target pattern

String that resolves to a set of [targets](#target). They can be used as arguments to commands such as `buck2 build` and `buck2 uquery`. They can also be used in the [visibility](#visibility) argument of a [rule](#rule). See more: [Target pattern](./target_pattern.md).

#### Package

Directory that contains a Buck2 [build file](#build-file) and all [source files](#source-file) belonging to the same directory as the build file or any of its subdirectories that do not contain a build file themselves.

#### Project

Outer most directory where there is a [.buckconfig](#buckconfig), also known as the [root cell](#cell). The .buckconfig for the project specifies the [cells](#cell) that constitute the Buck2 project. Specifically, these cells are specified in the '[repositories]' section of the .buckconfig. All command invocations are executed from the project root.

#### Provider

Data returned from a [rule](#rule) function. It's the only way that information from this rule is available to other rules that depend on it (see [dependency](#dependency)). Every rule must return at least the `DefaultInfo` provider. A common case is to also return either `RunInfo` (because they are executable) or custom providers that the dependents rule can use. See more: [Providers](https://buck2.build/docs/rule_authors/writing_rules/#providers).

#### Remote execution (RE)

Distributed execution of [actions](#action) on remote workers. It can speed up builds significantly, by scaling the nodes available for parallel actions and by caching action outputs across buck2 users.

#### Rule

A rule consists of an attribute spec and an implementation, which is a [Starlark](#starlark) function.

The attribute spec declares what attributes the rule expects to receive. The rule implementation receives the [attributes](#attribute) of a [target](#target) and the [providers](#provider) of its [dependencies](#dependency). It can declare new [actions](#action) and [artifacts](#artifact), and must return [providers](#provider) that can be used to pass data to its dependents or to Buck2 itself.

Rules are instantiated in [build files](#build-file) to declare targets and set their attributes. The rule implementation is called when Buck2 needs its providers, which can happen when the target is built, or when one of its dependents is.

As an example, you would use the `cxx_binary` rule to create a C++ binary, but you would use the `android_binary` rule to create an Android APK

#### Starlark
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Target
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Toolchain
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Transition
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Unconfigured graph
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Visibility
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
