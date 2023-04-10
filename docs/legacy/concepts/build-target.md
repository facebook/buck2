# Build Target

A *build target* is a string that identifies a build rule in your project. Build targets are used as arguments to Buck commands, such as [`buck build`](https://buck.build/command/build.html) and [`buck run`](https://buck.build/command/run.html). Build targets are also used as arguments to [build rules](https://buck.build/concept/build_rule.html) to enable one build rule to reference another. For example, a build rule might use a build target to reference another rule in order to specify that rule as a *dependency*.

#### Fully-qualified build targets

Here is an example of a *fully-qualified* build target:

```
//java/com/facebook/share:ui
```

A fully-qualified build target has three components:

1. The `//` prefix indicates that the subsequent path is from the *root* of your project. You can use the [`buck root`](https://buck.build/command/root.html) command to identify the root of your project.
2. The `java/com/facebook/share` between the `//` prefix and the colon (`:`) indicates that the [build file](https://buck.build/concept/build_file.html) (usually named `BUCK`) is located in the directory `java/com/facebook/share`.
3. The `ui` after the colon (`:`) indicates the name of the build rule within the build file. Build rule names must be unique within a build file. By *name* we mean, more formally, the value of the `name` argument to the build rule.

Note that the name of the build file itself—usually BUCK—does *not* occur in the build target. All build files within a given Buck project must have the same name—defined in the [`[buildfile].name`](https://buck.build/files-and-dirs/buckconfig.html#buildfile.name) entry of `.buckconfig`. Therefore, it is unnecessary to include the name in the target.
The full regular expression for a fully-qualified build target is as follows:

```
[A-Za-z0-9._-]*//[A-Za-z0-9/._-]*:[A-Za-z0-9_/.=,@~+-]+
|- cell name -|  | package path | |--- rule name ----|
```

In Buck, a *cell* defines a directory tree of one or more Buck packages. For more information about Buck cells and their relationship to packages and projects, see the [Key Concepts](https://buck.build/about/overview.html) topic.
**NOTE:** All target paths are assumed to start from the root of the Buck project. Buck does not support specifying a target path that starts from a directory below the root. Although the double forward slash (`//`) that prefixes target paths can be omitted when specifying a target from the command line (see **Pro Tips** below), Buck still assumes that the path is from the root. Buck does support *relative* build paths, but in Buck, that concept refers to specifying build targets *from within* a build file. See **Relative build targets** below for more details.

#### Relative build targets

A *relative* build target can be used to reference a [build rule](https://buck.build/concept/build_rule.html) *within the same *[*build file*](https://buck.build/concept/build_file.html). A relative build target starts with a colon (`:`) and is followed by only the third component (or *short name*) of the fully-qualified build target.
The following snippet from a build file shows an example of using a relative path.

```
## Assume this rule is in //java/com/facebook/share/BUCK#
java_binary(
  name = 'ui_jar',
  deps = [## The following target path##   //java/com/facebook/share:ui## is the same as using the following relative path.#':ui',],)
```

## Command-line Pro Tips

Here are some ways that you can reduce your typing when you specify build targets as command-line arguments to the [`buck build`](https://buck.build/command/build.html) or [`buck run`](https://buck.build/command/run.html) commands.
Consider the following example of a fully-qualified build target used with the `buck build` command:

```
buck build //java/com/facebook/share:share
```

Although Buck is always strict when parsing build targets in build files, Buck is flexible when parsing build targets on the command-line. Specifically, the leading `//` is optional on the command line, so the above could be:

```
buck build java/com/facebook/share:share
```

Also, if there is a forward slash before the colon, it is ignored, so this could also be written as:

```
buck build java/com/facebook/share/:share
```

which enables you to produce the red text shown below using tab-completion, which dramatically reduces how much you need to type:

```
buck build java/com/facebook/share/:share
```

Finally, if the final path element matches the value specified after the colon, it can be omitted:

```
# This is treated as //java/com/facebook/share:share.
buck build java/com/facebook/share/
```

which makes the build target even easier to tab-complete. For this reason, the name of the build rule for the primary deliverable in a build file is often named the same as the parent directory. That way, it can be built from the command-line with less typing.

## See also

Buck supports the ability to define ***aliases* for build targets**; using aliases can improve brevity when specifying targets on the Buck command line. For more information, see the [`[alias]`](https://buck.build/files-and-dirs/buckconfig.html#alias) section in the documentation for [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html).
A [**build target pattern**](https://buck.build/concept/build_target_pattern.html) is a string that describes a set of one or more build targets. For example, the pattern `//...` is used to build an entire project. For more information, see the **Build Target Pattern** topic.
