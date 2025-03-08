---
id: build_file
title: Build File
---

# Build File

A _build file_ is a file, typically named `BUCK`, that defines one or more
[build rule](build_rule.md)s. Buck2 takes a `BUCK` file as input and evaluates
the file to declare [target](build_target.md)s, which are then used to create a
graph of dependencies and to derive the actions that must be completed to build
intermediate and final software outputs.

## Syntax and structure overview

- Build files are syntactically Starlark files, containing a set of
  [target](build_target.md) definitions (i.e. invocations of functions where the
  name of the function is the type of the target, and the arguments to the
  function are the attributes of the target).
- Each build file can contain multiple target definitions and is uniquely
  identified by the directory it is in.

- Relative order of these target definitions is not important; all that matters
  is which target definitions were declared, and with what values, by the time
  evaluation of the build file completes.

- In order to enforce a clean separation between code and data, build files
  cannot contain arbitrary function definitions or conditional/for statements.
  Moreover, `*args` and `**kwargs` arguments are not permitted; instead, all the
  arguments must be listed explicitly. Instead, functions can be declared in
  `.bzl` files that must be loaded explicitly at the top of the build file.

### Mini BUCK example

Here is a mini example of a build file containing two targets, one refers to a
file `main.c` as its inputs and the other two files `greeting.c` and
`greeting.h`.

```python
cxx_binary(
    name = 'hello',
    srcs = [
        'main.c',
    ],
    deps = [
        ':greeting',
    ],
)

cxx_library(
    name = 'greeting',
    srcs = [
        'greeting.c',
    ],
    exported_headers = [
        'greeting.h',
    ],
)
```

### Targets

Each [target](build_target.md) has a name, identifying it uniquely in the same
build file. Additionally, it has a set of named attributes depending on the type
of the target. Attribute names can only have alphanumeric characters and
underscores, and cannot start with a digit.

## Build file naming and referencing

- You can change the name that Buck2 uses for the build file in the `buildfile`
  section of [buckconfig](glossary.md#buckconfig).

- A source file in your project can only be referenced by rules in its "nearest"
  build file, where "nearest" means its closest direct ancestor in your
  project's file tree. (If a source file has a build file as a sibling, then
  that is its nearest ancestor.) For example, if your project had the following
  `BUCK` files:

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

```python
java_library(
  name = 'collect',
  srcs = glob(['*.java']),
  deps = ['//java/com/facebook/base:base',],)
```

Then `java/com/facebook/common/BUCK` could have a rule like:

```python
java_library(
  name = 'concurrent',
  srcs = glob(['concurrent/*.java']),
  deps = ['//java/com/facebook/base:base','//java/com/facebook/common/collect:collect',],)
```

whereas the following **would be invalid** because
`java/com/facebook/common/collect/` has its own build file, so
`//java/com/facebook/common/collect:concurrent` cannot list
`java/com/facebook/common/collect/*.java` in its `srcs`.

```python
java_library(
  name = 'concurrent',
  srcs = glob(['collect/*.java', 'concurrent/*.java']),
  deps = ['//java/com/facebook/base:base',],)
```
