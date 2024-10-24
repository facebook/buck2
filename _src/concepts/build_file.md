---
id: build_file
title: Build File
---

# Build File

A _build file_ is a file, typically named `BUCK`, that defines one or more
[build rule](build_rule.md)s. Note that you can change the name that Buck2 uses
for the build file in the `buildfile` section of `.buckconfig`. A source file in
your project can only be referenced by rules in its "nearest" build file, where
"nearest" means its closest direct ancestor in your project's file tree. (If a
source file has a build file as a sibling, then that is its nearest ancestor.)
For example, if your project had the following `BUCK` files:

```
java/com/facebook/base/BUCK
java/com/facebook/common/BUCK
java/com/facebook/common/collect/BUCK
```

Then your build rules would have the following constraints:

- Rules in `java/com/facebook/base/BUCK` can reference any file under
  `java/com/facebook/base/`.
- Rules in `java/com/facebook/common/` can reference any files under that
  directory, except for those under `java/com/facebook/common/collect/`, as
  those "belong" to the `BUCK` file in the `collect` directory.

The set of source files accessible to a build file is also known as its _build
package_. The way to refer to code across build packages is to create build
rules and use `deps` to refer to that code. Going back to the previous example,
suppose code in `java/com/facebook/common/concurrent/` wants to depend on code
in `java/com/facebook/common/collect/`. Presumably
`java/com/facebook/common/collect/BUCK` has a build rule like:

```
java_library(
  name = 'collect',
  srcs = glob(['*.java']),
  deps = ['//java/com/facebook/base:base',],)
```

Then `java/com/facebook/common/BUCK` could have a rule like:

```
java_library(
  name = 'concurrent',
  srcs = glob(['concurrent/*.java']),
  deps = ['//java/com/facebook/base:base','//java/com/facebook/common/collect:collect',],)
```

whereas the following **would be invalid** because
`java/com/facebook/common/collect/` has its own build file, so
`//java/com/facebook/common/collect:concurrent` cannot list
`java/com/facebook/common/collect/*.java` in its `srcs`.

```
java_library(
  name = 'concurrent',
  srcs = glob(['collect/*.java', 'concurrent/*.java']),
  deps = ['//java/com/facebook/base:base',],)
```
