---
id: cheat_sheet
title: Cheat Sheet
---

# Buck2 Cheat Sheet

This section provides example command lines that you can use to obtain
information about Buck2 and about your build. These techniques can help you to
understand how your build works and to troubleshoot issues with your build.
These examples use the [`buck2 cquery`](../query/cquery) command. We recommend
cquery over uquery in most cases because cquery operates on the configured
graph, which means that targets have had the expected configurations applied on
them.

---

- How do I see the arguments for a given rule from the command line?
- How do I find all the targets for a package?
- How do I specify more than one target to `buck2 cquery`?
- How do I get the attribute names and values for the targets that result from a
  query?
- How do I perform a query inside of a rule?
- How do I find the dependencies for a target, that is, the targets on which a
  specified target depends?
- How do I find the reverse-dependencies for a target, that is, the targets that
  depend on a specified target?
- How do I find the build file that contains the target that owns a source file?

---

### How do I find all the targets for a package?

Specify a _build target pattern_ that represents the targets in the package.

```
buck2 cquery //path/to/dir/...
```

The `buck2 cquery` command can accept a
[build target pattern](../../concepts/target_pattern) as a parameter. If you
specify a build target pattern, Buck2 evaluates this pattern and shows all the
build targets that match it.

### How do I specify more than one target to `buck2 cquery`?

Use the `buck2 cquery set()` operator. The following command line returns the
target `main` in the build file in the root of the Buck2 project and all the
targets from the build file in the `myclass` subdirectory of the root.

```
buck2 cquery "set( '//:main' '//myclass:' )"
```

### How do I get the attribute names and values for the targets returned by a query?

Add the `--output-attribute <ATTRIBUTE>` or `--output-all-attributes` option to
the command line, followed by regular expressions that represent the attributes
of interest.

```
buck2 cquery "deps(//foo:bar)" --output-attribute 'name' 'exported_headers'
```

The `--output-attribute` option enables you to specify which attributes Buck2
should return. Instead of returning the names of the targets that match the
query expression, Buck2 returns the names and values of the specified attributes
for those targets in JSON format. Attributes are specified as regular
expressions. For example, `'.*'` matches all attributes. See the
[`buck2 cquery` docs](../query/cquery) for more details. The output for the
example query above might look something like the following.

```
{"//foo/bar/lib:lib" : {"exported_headers" : [ "App/util.h" ],"name" : "lib"},"//foo/bar:app" : {"exported_headers" : [ "App/lib.h" ],"name" : "app"}}
```

### How do I perform a query** \***inside**\* **of a rule?

Buck2 supports certain string parameter macros to be used when defining a
target. You can use the query macros as such:

```
$(query_targets "queryfunction(//:foo)")
$(query_outputs "queryfunction(//:foo)")
$(query_targets_and_outputs [SEPARATOR] "queryfunction(//:foo)")
```

Note, however, that the query macros are supported only for
[`genrule`](../../api/rules/#genrule) and
[`apk_genrule`](../../api/rules/#apk_genrule).

### How do I find the dependencies for a target?

Use the `deps()` operator.

```
buck2 cquery "deps('//foo:bar')"
buck2 cquery "deps('//foo:bar', 1, first_order_deps())"
buck2 cquery "deps(set('//foo:bar' '//foo:lib' '//foo/baz:util'))"
```

The `deps` operator finds the dependencies of the specified targets. The first
argument represents the targets of interest. This can be a single
[build target](../../concepts/build_target) or
[build target pattern](../../concepts/target_pattern), or a set of these. The
optional second argument is the _depth_ of the search for dependencies from the
specified targets. For example, `1`, as shown in the example above, returns only
the direct dependencies. If you do not provide this argument, the output is the
complete set of transitive dependencies. How do I find the reverse-dependencies
for a target, that is, the targets that** \***depend on**\* **a specified
target? Use the `buck2 cquery rdeps()` (reverse dependencies) operator. The
following example, returns the targets in the
[transitive closure](https://en.wikipedia.org/wiki/Transitive_closure) of
`//foo:bar` that depend directly on `//example:baz`.

```
buck2 cquery "rdeps('//foo:bar', '//example:baz', 1)"
```

### How do I find the buildfile that contains the target that owns a source file?

In order to find the build file associated with a source file, combine the
`owner` operator with `buildfile`. For example,

```
buck2 cquery "buildfile(owner('foo/bar/main.cpp'))"
```

first finds the targets that _own_ `foo/bar/main.cpp` and then returns the build
files, such as `foo/bar/BUCK`, that define those targets.
