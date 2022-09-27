# Build Rule

A *build rule* is a procedure for producing output files from a set of input files in the context of a specified build configuration. Build rules are specified in [build file](https://buck.build/concept/build_file.html)s—typically named BUCK.
**Note:** A build rule must explicitly specify, in its arguments, all of its required inputs in order for Buck to be able to build the rule's output in a way that is deterministic and reproducible.

## Buck's collection of build rules

Buck comes with a collection of built-in build rules for many common build procedures. For example, compiling Java code against the Android SDK is a common procedure, so Buck provides the build rule [`android_library`](https://buck.build/rule/android_library.html) to do that. Similarly, the final product of most Android development is an APK, so you can use the build rule [`android_binary`](https://buck.build/rule/android_binary.html) to create an APK.
This documentation organizes Buck's build rules by development language and by target platform. Examples are: **C++**, **Java**, **Python** (development languages) and **Android**, **iOS**, **.NET** (target platforms). Consult the table of contents to locate the build rules that are appropriate for your development project.
You can view a list of Buck's build rules from the command line with the command:

```
buck audit ruletypes
```

You can view the arguments supported by a particular rule with the command:

```
buck audit ruletype <rule>
```

Note that the first of these commands uses the *plural* `ruletypes`, and the second uses the *singular* `ruletype`. For more information, see the [`buck audit`](https://buck.build/command/audit.html) documentation.

## Source files as inputs to build rules

Most build rules specify source files as inputs. For example, a [`cxx_library`](https://buck.build/rule/cxx_library.html) rule would specify `.cpp` files as inputs. To support specifying these files, a `cxx_library` rule provides the `srcs` argument. Some languages, such as C++, use header files as well. To specify these, `cxx_library` provides a `headers` argument.
In addition to `srcs` and `headers`, some rules provide variants of these arguments, such as `platform_srcs` and `platform_headers`. These arguments support groups of source files that should be used as inputs only when building for specific platforms. For more information, see the descriptions for `platform_srcs` and `platform_headers` in, for example, the [`cxx_library`](https://buck.build/rule/cxx_library.html) topic.

### Package boundaries and access to source files

In Buck, a BUCK file defines a *package*, which corresponds *roughly* to the directory that contains the BUCK file and those subdirectories that do not themselves contain BUCK files. (To learn more, see the [Key Concepts](https://buck.build/about/overview.html) topic.)
A rule in a BUCK file cannot specify a source file as an input unless that source file is in that BUCK file's package. An exception to this restriction exists for header files, but only if a rule in the package that contains the header file *exports* that header file using the `exported_headers` argument. For more details, see the description for `exported_headers` in, for example, the [`cxx_library`](https://buck.build/rule/cxx_library.html) topic.
More commonly though, the package for a BUCK file contains all the source files required for the rules defined in that BUCK file. Functionality in source files from other packages is made available through the artifacts produced by the rules in the BUCK files for those packages. For example, a [`cxx_binary`](https://buck.build/rule/cxx_binary.html) might use the functionality in a `cxx_library` that is defined in another package. To access that functionality, the `cxx_binary` would take that `cxx_library` as a *dependency*.

##### Symlinks: Use with caution if at all

We recommend that you do *not* use symlinks—either absolute or relative—to specify input files to build rules. Although using symlinks in this context does sometimes work, it can lead to unexpected behavior and errors.

## Dependencies: Output from one rule as input to another rule

A build rule can use the output from another build rule as one of its inputs by specifying that rule as a *dependency*. Typically, a build rule specifies its dependencies as a list of [build target](https://buck.build/concept/build_target.html)s in its `deps` argument. However, the rule can also specify dependencies—as build targets—in other arguments, such as `srcs`.
**Example:** The output of a [`java_library`](https://buck.build/rule/java_library.html) rule is a JAR file. If a `java_library` rule specifies another `java_library` rule as a dependency, the JAR file produced by the specified rule is added to the classpath for the `java_library` that depends on it.
**Example:** If a [`java_binary`](https://buck.build/rule/java_binary.html) rule specifies a `java_library` rule as a dependency, the JAR file for the specified `java_library` is available on the classpath for the `java_binary`. In addition, in the case of `java_binary`, the JAR files for any dependencies of the `java_library` rule *are also* made available to the `java_binary` rule—and if those dependencies have dependencies of their own, they are added as well. This exhaustive cascade of dependencies is referred to as the rule's *transitive closure*.

### Required dependencies are always built first

Buck guarantees that any dependencies that a rule lists that are required in order to build that rule are built successfully *before* Buck builds the rule itself. Note though that there can be special cases—such as [`apple_bundle`](https://buck.build/rule/apple_bundle.html)—where a rule's listed dependencies do not actually need to be built before the rule.

### Visibility

In order for a build rule to take a dependency on another build rule, the build rule on which the dependency is taken must be *visible* to the build rule taking the dependency. A build rule's `visibility` argument is a list of [build target pattern](https://buck.build/concept/build_target_pattern.html)s that specify the rules that can take that rule as a dependency. For more information about the concept of visibility in Buck, see the [Visibility](https://buck.build/concept/Visibility.html) topic.

### Dependencies define a graph

Build rules and their dependencies define a directed acyclic graph (DAG). Buck requires this graph to be acyclic to make it possible to build independent subgraphs in parallel.

## How to handle special cases: genrules and macros

Although Buck provides a rich set of built-in build rules for developers, it is not able to address all possible needs. As an "escape hatch," Buck provides a category of generic build rules called *genrules*. With genrules, you can perform arbitrary operations using shell scripts. The genrules supported by Buck are:

* [`genrule`](https://buck.build/rule/genrule.html)
* [`apk_genrule`](https://buck.build/rule/apk_genrule.html)
* [`cxx_genrule`](https://buck.build/rule/cxx_genrule.html)

### Multiple output files with genrules

In most cases, a build rule produces exactly one output file. However, with genrules, you can specify an output *directory* and write arbitrary files to that directory.

### Macros

Finally, note that you can define functions that generate build rules. In general, this should not be something that you need to do, but taking advantage of this option might help you add needed functionality to Buck's without editing its source code. For more details, see the [Custom Macros](https://buck.build/extending/macros.html) topic.
