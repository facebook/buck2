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
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Build
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Build file
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Bxl
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
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
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Execution platform
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
#### Isolation dir
:::note
🚧   THIS SECTION IS UNDER CONSTRUCTION
:::
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
