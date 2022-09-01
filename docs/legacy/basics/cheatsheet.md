# Buck Cheat Sheet

This section provides example command lines that you can use to obtain information about Buck and about your build. These techniques can help you to understand how your build works and to troubleshoot issues with your build.
Most of these examples use the [`buck query`](https://buck.build/command/query.html), [`buck targets`](https://buck.build/command/targets.html), and [`buck audit`](https://buck.build/command/audit.html) commands. For more information and examples, see the reference pages for those commands.
* * *

* How do I get a list of all the rules that Buck supports from the command line?
* How do I see the arguments for a given rule from the command line?
* How do I find all the targets for a package?
* How do I specify more than one target to buck query?
* How do I get the attribute names and values for the targets that result from a query?
* How do I perform a query inside of a rule?
* How do I find the dependencies for a target, that is, the targets on which a specified target depends?
* How do I find the reverse-dependencies for a target, that is, the targets that depend on a specified target?
* How do I find the build file that contains the target that owns a source file?

* * *

### How do I get a list of all the rules that Buck supports,** ***from the command line*, so that I can process them with** **`grep`,** **`sed`, etc?

Use [`buck audit`](https://buck.build/command/audit.html) with the `ruletypes` (plural) subcommand, which returns an alphabetized list of all the rules that Buck supports.
The following command line uses `buck audit ruletypes` with the `grep` command to print all the build rules that have the string `android` in their names.

```
buck audit ruletypes | grep android
```

Note that these are not all the rules that Buck provides for Android development. For example, the rules `apk_genrule` and `ndk_library` support Android development, but do not themselves contain the string `android` in their names.
How do I see the arguments for a rule from the command line?
Use [`buck audit`](https://buck.build/command/audit.html) with the `ruletype` (singular) subcommand followed by the name of the rule.
The following command line uses `buck audit ruletype` to view the arguments supported by the [`remote_file`](https://buck.build/rule/remote_file.html) rule.

```
buck audit ruletype remote_file
def remote_file (
    name,
    sha1,
    url,
    labels = None,
    licenses = None,
    out = None,
    type = None,
):
    ...
```

### How do I find all the targets for a package?

Specify a *build target pattern* that represents the targets in the package.

```
buck query //path/to/dir/...
```

The `buck query` command can accept a [build target pattern](https://buck.build/concept/build_target_pattern.html) as a parameter. If you specify a build target pattern, Buck evaluates this pattern and shows all the build targets that match it.
How do I specify more than one target to** **`buck query`?
Use the [`buck query set()`](https://buck.build/command/query.html#set) operator.
The following command line returns the target `main` in the build file in the root of the Buck project and all the targets from the build file in the `myclass` subdirectory of the root.

```
buck query "set( '//:main' '//myclass:' )"
```

### How do I get the attribute names and values for the targets returned by a query?

Add the `--output-attributes` option to the command line, followed by regular expressions that represent the attributes of interest.

```
buck query "deps(//foo:bar)" --output-attributes 'name' 'exported_headers'
```

The `--output-attributes` option enables you to specify which attributes Buck should return. Instead of returning the names of the targets that match the query expression, Buck returns the names and values of the specified attributes for those targets in JSON format. Attributes are specified as regular expressions. For example, `'.*'` matches all attributes. See the [buck query page](https://buck.build/command/query.html#output-attributes) for more details. The output for the example query above might look something like the following.

```
{"//foo/bar/lib:lib" : {"exported_headers" : [ "App/util.h" ],"name" : "lib"},"//foo/bar:app" : {"exported_headers" : [ "App/lib.h" ],"name" : "app"}}
```

### How do I perform a query** ***inside*** **of a rule?

Use [**string parameter macros**](https://buck.build/function/string_parameter_macros.html), specifically, the *query* macros:

```
$(query_targets "queryfunction(//:foo)")
$(query_outputs "queryfunction(//:foo)")
$(query_targets_and_outputs [SEPARATOR] "queryfunction(//:foo)")
```

Note, however, that the query macros are supported only for [`genrule`](https://buck.build/rule/genrule.html) and [`apk_genrule`](https://buck.build/rule/apk_genrule.html).

### How do I find the dependencies for a target?

Use the `deps()` operator.

```
buck query "deps('//foo:bar')"
buck query "deps('//foo:bar', 1, first_order_deps())"
buck query "deps(set('//foo:bar' '//foo:lib' '//foo/baz:util'))"
```

The [deps](https://buck.build/command/query.html#deps) operator finds the dependencies of the specified targets. The first argument represents the targets of interest. This can be a single [build target](https://buck.build/concept/build_target.html) or [build target pattern](https://buck.build/concept/build_target_pattern.html), or a set of these.
The optional second argument is the *depth* of the search for dependencies from the specified targets. For example, `1`, as shown in the example above, returns only the direct dependencies. If you do not provide this argument, the output is the complete set of transitive dependencies.
How do I find the reverse-dependencies for a target, that is, the targets that** ***depend on*** **a specified target?
Use the `buck query` [`rdeps`](https://buck.build/command/query.html#rdeps) (reverse dependencies) operator.
The following example, returns the targets in the [transitive closure](https://en.wikipedia.org/wiki/Transitive_closure) of `//foo:bar` that depend directly on `//example:baz`.

```
buck query "rdeps('//foo:bar', '//example:baz', 1)"
```

### How do I find the buildfile that contains the target that owns a source file?

In order to find the build file associated with a source file, combine the `owner` operator with `buildfile`. For example,

```
buck query "buildfile(owner('foo/bar/main.cpp'))"
```

first finds the targets that *own* `foo/bar/main.cpp` and then returns the build files, such as `foo/bar/BUCK`, that define those targets.
