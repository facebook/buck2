# Skylark

## A bit of history

Historically, Buck relied on a Python DSL to describe [build file](https://buck.build/concept/build_file.html)s and [macros](https://buck.build/extending/macros.html). This enabled Buck users to implement many features without having to modify Buck's core. Although Python worked fine for local builds and small repositories, when used at scale, the ability to access the host environment and perform arbitrary actions without Buck's knowledge led to non-deterministic builds, hard-to-debug issues, and slow parsing.
To address some of these issues, we introduced features such as [`allow_unsafe_import()`](https://buck.build/function/allow_unsafe_import.html), but ultimately we were unable to provide proper sandboxing for deterministic parsing, and a new solution had to be put in place.

## Present day

In order to tackle the limitations of the Python DSL parser, we added multiple-language support and a built-in parser for the [Skylark](https://docs.bazel.build/versions/master/skylark/language.html) language. The new parser provides an alternative to the Python DSL parser.

## Enabling the Skylark parser

In order to enable Skylark support for your project, add the following key to the [`[parser]`](https://buck.build/files-and-dirs/buckconfig.html#parser) section in your`.buckconfig` file.

```
[parser]
  default_build_file_syntax = SKYLARK
```

We recommend Skylark for new projects and it will become the default in the future. However, if most of your [build file](https://buck.build/concept/build_file.html)s or [macros](https://buck.build/extending/macros.html) rely on Python DSL features and you're not ready to invest in migrating to Skylark, replace

```
default_build_file_syntax = SKYLARK
```

with

```
default_build_file_syntax = PYTHON_DSL
```

to use the Python DSL parser by default.
If your project includes build files that rely on legacy Python DSL features, you can enable *multi-language* support by setting [`[parser].polyglot_parsing_enabled`](https://buck.build/files-and-dirs/buckconfig.html#parser.polyglot_parsing_enabled) to `true` in [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html) file and use the file-specific parser directives described below.
We recommend that you migrate Skylark as soon as possible. To make that easier, Buck gives you control over which parser it uses for individual [build file](https://buck.build/concept/build_file.html)s. If you add

```
# BUILD FILE SYNTAX: SKYLARK
```

as the first line of a [build file](https://buck.build/concept/build_file.html), Buck uses the Skylark parser. If instead, you add

```
# BUILD FILE SYNTAX: PYTHON_DSL
```

then Buck uses the Python DSL parser.
If neither of these lines is present, Buck uses the parser specified in the [`[parser]`](https://buck.build/files-and-dirs/buckconfig.html#parser) section of `.buckconfig`.
Because Skylark will eventually become the default, it's best to enable the Skylark parser globally in `.buckconfig` and add

```
# BUILD FILE SYNTAX: PYTHON_DSL
```

to any [build file](https://buck.build/concept/build_file.html)s that continue to rely on Python DSL features.
**Note:** The `# BUILD FILE SYNTAX:` parser directive is recognized in build files only if support for multi-language (polyglot) parsing is enabled in `.buckconfig`:

```
[parser]
  polyglot_parsing_enabled = true
```

## Migrating from Python to Skylark

The [Skylark](https://docs.bazel.build/versions/master/skylark/language.html) language was specifically created to address the issues mentioned previously—as well as other issues—which is why Skylark will eventually replace the Python DSL as the language for [build file](https://buck.build/concept/build_file.html)s and extension files. Unfortunately, migration cannot be fully automated, so here we describe some ways to resolve common issues when migrating from the Python DSL to Skylark.

### Like Python, but...

As Skylark is a subset of Python, there are several features that have been removed. For features that have been removed like top level conditionals, unbounded loops, and others, there are design justifications available in the [specification's repository](https://github.com/bazelbuild/starlark/blob/master/design.md).

### include_defs

The [`include_defs()`](https://buck.build/function/include_defs.html) function is not supported in Skylark because it can contaminate the symbol table of the execution environment and make automated refactoring more difficult.
To replace an invocation such as

```
include_defs("//tools/my_macro.bzl")
```

you should:

1. find all symbols defined in the `my_macro.bzl` file that are *actually used* by the including file, say, for example, `foo` and `bar`.
2. replace the `include_defs` invocation with an equivalent [`load()`](https://buck.build/function/load.html)invocation that *explicitly* imports the needed symbols:

```
load("//tools:my_macro.bzl", "foo", "bar")
```

**Note:** The [`load()`](https://buck.build/function/load.html) function uses the [build target pattern](https://buck.build/concept/build_target_pattern.html) syntax as though

```
export_file(name="my_macro.bzl")
```

were defined in a `tools` package [build file](https://buck.build/concept/build_file.html). This means that instead of using the `//package/extension.bzl` syntax expected by [`include_defs()`](https://buck.build/function/include_defs.html), you should use the `//package:extension.bzl` syntax expected by [`load()`](https://buck.build/function/load.html).

### Environment variables

For Skylark, replace environment variables with equivalent configuration variables. The implicit nature of environment variables frequently results in non-reproducible builds because of differences in the values of environment variables across machines.
For example, in your [build file](https://buck.build/concept/build_file.html) or extension file, instead of

```
my_var = py_sdk.os.env.get('MY_VAR', 'foo')
```

use

```
my_var = read_config('my_project', 'my_var', 'foo')
```

Then, when calling Buck, instead of passing

```
export MY_VAR='some_value'
buck <args>
```

pass a configuration flag

```
buck <args> --config my_project.my_var=foo
```

or better yet, define these configuration values in your [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html) file.
**Note:** When using the Python DSL parser it's possible to invoke the [`read_config()`](https://buck.build/function/read_config.html) function directly during extension-file evaluation or indirectly through other function invocations. Indirect invocation of [`read_config()`](https://buck.build/function/read_config.html) is not supported with the Skylark parser in order to track the use of configuration options more precisely. Because of this, a top-level invocation of [`read_config()`](https://buck.build/function/read_config.html) such as:

```
bar = read_config(<args>)
```

either has to be performed in a [build file](https://buck.build/concept/build_file.html) directly or, preferably, moved into a descriptively-named function within an extension file. In the case where configuration options are used to instantiate expensive objects which should be created only once, consider replacing a top-level invocation such as

```
FOO = expensive1() if read_config(<args>) else expensive2()
```

with something like

```
_EXPENSIVE1 = expensive1()
_EXPENSIVE2 = expensive2()

def foo():
  return _EXPENSIVE1 if read_config(<args>) else _EXPENSIVE2
```

While it can result in the instantiation of an unnecessary and expensive object, it might still be more efficient than instantiating one of the expensive objects during each`foo` invocation. Having said that, we recommend that you start simply and optimize only if you notice performance issues.

### isinstance()

The `isinstance()` function is not available in Skylark because Skylark does not support inheritance. However, some usages of `isinstance()` can be replaced with the `type` function. For example,

```
isinstance(foo, str)
```

can be replaced with

```
type(foo) == type('str')
```

### get_base_path

In Skylark, we've replaced the [`get_base_path()`](https://buck.build/function/get_base_path.html)function with the equivalent—but more appropriately named—[package_name()](https://docs.bazel.build/versions/master/skylark/lib/native.html#package_name) function. Note that in [build file](https://buck.build/concept/build_file.html)s, it's invoked as `package_name()`, but in extension files, it's invoked as `native.package_name()`. Using the `native` prefix is consistent with the rest of the built-in functions provided by Buck. If there is a strong desire to use the old name instead, you can assign the new function to the legacy function name:

```
get_base_path = native.package_name
get_base_path()
```

### del

Usage of `del arr[1]` and `del dictionary['key']` are not supported. Instead, use

```
arr_val  = arr.pop(1)
dict_val = dictionary.pop('key')
```

### class

Classes are not supported. Replace classes with a combination of structs and functions. In addition to being simpler, structs are more [memory efficient](http://blog.explainmydata.com/2012/07/expensive-lessons-in-python-performance.html). For example, a class such as

```
class Foo:
def __init__(self, foo, bar=None):
...
def some_method(self, param):
...
...
foo = Foo('foo', bar='yo')
res = foo.some_method(some_param)
```

can be replaced with something such as

```
def some_function(foo_instance, param):
...
foo = struct(foo='foo', bar='yo')
res = some_function(foo, some_param)
```

You can also track state in variables defined in the same extension file, but you cannot expose any mutators, since all variables are immutable once the extension file is evaluated. This is intentional and prevents race conditions because build files as well as extension files must support efficient parallel evaluation.

### Regular expressions (import re)

Regular expressions are not supported in Skylark due to unbounded runtime and resource usage, but often regular expressions can be replaced with string functions.

##### Example: Match characters at the end of a string

Replace

```
re"//libraries/my_lib/.*"
```

with

```
startswith("//libraries/my_lib/")
```

##### Example: Match characters at the beginning of a string

Replace

```
re".*/my_lib/"
```

with

```
endswith("/my_lib/")
```

##### Example: Match characters at both the beginning and end of a string

Replace

```
re".*some_text.*"
```

with

```
"some_text" in foo
```

### raise Exception

Raising and catching exceptions is not supported. Instead, use the [`fail`](https://docs.bazel.build/versions/master/skylark/lib/globals.html#fail) function to stop the evaluation of a build or extension file, and report an error.
For example, instead of

```
raise Exception("foo")
```

use

```
fail("foo")
```

Instead of

```
raise Exception("attribute_name: foo")
```

use

```
fail("foo", "attribute_name")
```

Since usage of `fail` triggers non-recoverable errors and halts parsing, it cannot be used for control flow.

### while loop

While loops are not supported due to unbounded runtime. Instead, use a `for` loop with a bounded range. Usage of

```
while True: ...
```

should be replaced with

```
for _ in range(<reasonable limit>):
```

followed by an extra check after the loop to make sure the loop terminated before all the iterations were exhausted.

### python module

Python modules cannot be imported in Skylark. However, you can replace many safe Python functions with analogous functions from [Skylib](https://github.com/bazelbuild/bazel-skylib). For example, you can replace `os.path.basepath` with `paths.basename`, and you can replace `os.path.join` with `paths.join`.
In order to use Skylib, clone its repository from GitHub into a local directory. Then, configure that repository as a Buck cell by adding

```
[repositories]
  bazel_skylib = path/to/skylib_checkout
```

to your `.buckconfig` file.
Load the functions that you need, using [`load()`](https://buck.build/function/load.html). For example,

```
load("@bazel_skylib//lib:paths.bzl", "paths").
```

Here is an example from the Skylib website:

```
load("@bazel_skylib//:lib.bzl", "paths", "shell")

p = paths.basename("foo.bar")
s = shell.quote(p)
```

### Skylint

Consider running the [Skylint](https://github.com/bazelbuild/bazel/blob/master/site/docs/skylark/skylint.md) linting tool on your extension (`.bzl`) files. Skylint can catch many common issues and suggest fixes. Unfortunately, some constructs in Python can cause Skylint to crash. Some examples are:

* Nested functions. Nested functions should be moved to top-level scope.
* Usage of `not foo in`. You should instead use `foo not in`. Note that `foo not in` is recommended by the [flake8](http://flake8.pycqa.org/en/latest/) style-enforcement tool.

When debugging an issue, an effective strategy is to bisect your code by commenting out parts of the file and rerunning Skylint.

### Testing your changes

The easiest way to verify that your changes have not affected your build rules is by checking if the corresponding rule keys have changed. Before making your changes, capture rule keys with the following command:

```
buck targets --show-rulekey //path/to/targets/... > before
```

After making your changes, run the command again, redirecting to a different file.

```
buck targets --show-rulekey //path/to/targets/... > after
```

Now that you have captured the before and after rule keys, use the following command to compare them:

```
diff before after
```

There should be no differences unless your changes affected the semantics of some of the build definitions or macros. In order to get more insight into what exactly has changed, you can use the command

```
buck audit rules path/to/BUCK command
```

on individual [build file](https://buck.build/concept/build_file.html)s to see how Buck expanded the macros in them.
