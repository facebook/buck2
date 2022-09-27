# Build Target Pattern

A *build target pattern* is a string that describes a set of one or more [build target](https://buck.build/concept/build_target.html)s. You can use build target patterns as arguments to commands, such as [`buck build`](https://buck.build/command/build.html) and [`buck query`](https://buck.build/command/query.html). You can also use build target patterns in the [Visibility](https://buck.build/concept/visibility.html) argument of your build rules.
The simplest build target pattern matches the build target of the same name:

```
#
# Matches //apps/myapp:app
#
//apps/myapp:app
```

A build target pattern that ends with a colon matches all build targets in the build file at the preceding directory path. For example, suppose that the build file

```
apps/myapp/BUCK
```

defines the rules: `app_debug` and `app_release`, then the following build target pattern would match both of those rules:

```
#
# Matches //apps/myapp:app_debug and //apps/myapp:app_release
#
//apps/myapp:
```

A build target pattern that ends with an ellipsis (`/...`) matches all build targets in the build file in the directory that precedes the ellipsis and also *all build targets in build files in subdirectories*. For example, suppose that you have the following build files:

```
apps/BUCK
apps/myapp/BUCK
```

then the following pattern would match all build targets in both of those files:

```
#
# Matches (for example) //apps:common and //apps/myapp:app
#
//apps/...
```

### Build target patterns are not allowed in the deps argument

Build target patterns cannot be used with the `deps` argument of a build rule. Buck requires that you specify all dependencies explicitly as either fully-qualified or relative build targets.
By making dependencies explicit, Buck prevents build rules from *inadvertently* adding new dependencies, which can result in non-reproducible builds. In addition, if the added dependencies are not actually required, they can unnecessarily drive up the computational cost of the build.

### Target aliases

Buck supports the ability to define *aliases* for build targets; using aliases can improve brevity when specifying targets on the Buck command line. For more information, see the [`[alias]`](https://buck.build/files-and-dirs/buckconfig.html#alias) section in the documentation for [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html).
