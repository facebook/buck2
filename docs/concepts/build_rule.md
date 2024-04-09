---
id: build_rule
title: Build Rule
---

# Build Rule

A _build rule_ is a procedure for producing output files from a set of input
files in the context of a specified build configuration. Build rules are
specified in [build file](build_file.md)s—typically named BUCK. **Note:** A
build rule must explicitly specify, in its arguments, all of its required inputs
in order for Buck2 to be able to build the rule's output in a way that is
deterministic and reproducible.

## Buck2's collection of build rules

Buck2 comes with a collection of built-in build rules for many common build
procedures. For example, compiling Java code against the Android SDK is a common
procedure, so Buck2 provides the build rule
[`android_library`](../../api/rules/#android_library) to do that. Similarly, the
final product of most Android development is an APK, so you can use the build
rule [`android_binary`](../../api/rules/#android_binary) to create an APK.

## Source files as inputs to build rules

Most build rules specify source files as inputs. For example, a
[`cxx_library`](../../api/rules/#cxx_library) rule would specify `.cpp` files as
inputs. To support specifying these files, a `cxx_library` rule provides the
`srcs` argument. Some languages, such as C++, use header files as well. To
specify these, `cxx_library` provides a `headers` argument. In addition to
`srcs` and `headers`, some rules provide variants of these arguments, such as
`platform_srcs` and `platform_headers`. These arguments support groups of source
files that should be used as inputs only when building for specific platforms.

### Package boundaries and access to source files

In Buck2, a BUCK file defines a _package_, which corresponds _roughly_ to the
directory that contains the BUCK file and those subdirectories that do not
themselves contain BUCK files. (To learn more, see the
[Key Concepts](key_concepts.md) topic.) A rule in a BUCK file cannot specify a
source file as an input unless that source file is in that BUCK file's package.
An exception to this restriction exists for header files, but only if a rule in
the package that contains the header file _exports_ that header file using the
`exported_headers` argument. For more details, see the description for
`exported_headers` in, for example, the
[`cxx_library`](../../api/rules/#cxx_library) topic. More commonly though, the
package for a BUCK file contains all the source files required for the rules
defined in that BUCK file. Functionality in source files from other packages is
made available through the artifacts produced by the rules in the BUCK files for
those packages. For example, a [`cxx_binary`](../../api/rules/#cxx_binary) might
use the functionality in a `cxx_library` that is defined in another package. To
access that functionality, the `cxx_binary` would take that `cxx_library` as a
_dependency_.

##### Symlinks: Use with caution if at all

We recommend that you do _not_ use symlinks—either absolute or relative—to
specify input files to build rules. Although using symlinks in this context does
sometimes work, it can lead to unexpected behavior and errors.

## Dependencies: Output from one rule as input to another rule

A build rule can use the output from another build rule as one of its inputs by
specifying that rule as a _dependency_. Typically, a build rule specifies its
dependencies as a list of [build target](build_target.md)s in its `deps`
argument. However, the rule can also specify dependencies—as build targets—in
other arguments, such as `srcs`. **Example:** The output of a
[`java_library`](../../api/rules/#java_library) rule is a JAR file. If a
`java_library` rule specifies another `java_library` rule as a dependency, the
JAR file produced by the specified rule is added to the classpath for the
`java_library` that depends on it. **Example:** If a
[`java_binary`](../../api/rules/#java_binary) rule specifies a `java_library`
rule as a dependency, the JAR file for the specified `java_library` is available
on the classpath for the `java_binary`. In addition, in the case of
`java_binary`, the JAR files for any dependencies of the `java_library` rule
_are also_ made available to the `java_binary` rule—and if those dependencies
have dependencies of their own, they are added as well. This exhaustive cascade
of dependencies is referred to as the rule's _transitive closure_.

### Required dependencies are always built first

Buck2 guarantees that any dependencies that a rule lists that are required in
order to build that rule are built successfully _before_ Buck2 builds the rule
itself. Note though that there can be special cases—such as
[`apple_bundle`](../../api/rules/#apple_bundle)—where a rule's listed
dependencies do not actually need to be built before the rule.

### Visibility

In order for a build rule to take a dependency on another build rule, the build
rule on which the dependency is taken must be _visible_ to the build rule taking
the dependency. A build rule's `visibility` argument is a list of
[build target pattern](target_pattern.md)s that specify the rules that can take
that rule as a dependency. For more information about the concept of visibility
in Buck2, see the [Visibility](visibility.md) topic.

### Dependencies define a graph

Build rules and their dependencies define a directed acyclic graph (DAG). Buck2
requires this graph to be acyclic to make it possible to build independent
subgraphs in parallel.

## How to handle special cases: genrules and macros

Although Buck2 provides a rich set of built-in build rules for developers, it is
not able to address all possible needs. As an "escape hatch," Buck2 provides a
category of generic build rules called _genrules_. With genrules, you can
perform arbitrary operations using shell scripts. The genrules supported by
Buck2 are:

- [`genrule`](../../api/rules/#genrule)
- [`apk_genrule`](../../api/rules/#apk_genrule)
- [`cxx_genrule`](../../api/rules/#cxx_genrule)

### Multiple output files with genrules

In most cases, a build rule produces exactly one output file. However, with
genrules, you can specify an output _directory_ and write arbitrary files to
that directory.

### Macros

Finally, note that you can define functions that generate build rules. In
general, this should not be something that you need to do, but taking advantage
of this option might help you add needed functionality to Buck2's without
editing its source code.

## String parameter macros

It is also possible to expand references to other rules within the `cmd`, using
builtin `string parameter macros`. All build rules expanded in the command are
automatically considered to be dependencies of the `genrule()`.

Note that the paths returned by these macros are _relative_ paths. Using
relative paths ensures that your builds are _hermetic_, that is, they are
reproducible across different machine environments.

`$(classpath //path/to:target)`

Expands to the transitive classpath of the specified build rule, provided that
the rule has a Java classpath. If the rule does not have (or contribute to) a
classpath, then an exception is thrown and the build breaks.

`$(exe //path/to:target)`

Expands a build rule that results in an executable to the commands necessary to
run that executable. For example, a `java_binary()` might expand to a call to
`java -jar path/to/target.jar` . Files that are executable (perhaps generated by
a `genrule()`) are also expanded. If the build rule does not generate an
executable output, then an exception is thrown and the build breaks.

If the `$(exe my_dependency)` dependency should actually be built with the
target platform, use `$(exe_target my_dependency)` instead, which will stick to
the same platform as the target.

`$(location //path/to:target)`

Expands to the location of the output of the specified build rule. This means
that you can refer to the output without needing to be aware of how Buck is
storing data on the disk mid-build.

```

```
