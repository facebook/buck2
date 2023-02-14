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
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Package
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Project
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Provider
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Remote execution (RE)
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Rule
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Source file
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
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
