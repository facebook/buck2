---
id: loading_data
title: Loading Data in Starlark
---

You can load static data from within `BUCK` or `.bzl` files, as long as that
static data is stored in a JSON or TOML file in a package.

```python
load("//foo:bar.toml", "value")

some_rule(
    name = "my_rule",
    data = value,
)
```

This is useful for example if the data being loaded is generated outside of the
build process, but is necessary for the build itself - like URLs and checksums
being passed into
[`http_archive`](https://buck2.build/docs/prelude/rules/core/http_archive/).

By default the file name must end in `.toml` or `.json`, and `load()` can only
import the name `value` from these files. For TOML, `value` is always going to be
a dictionary with `str` keys (i.e. a
[TOML table](https://toml.io/en/v1.0.0#table)), and for JSON it maps to whatever
value is in the file.

## Files with a different extension

Plenty of data files do not have a usable extension - `Cargo.lock` is TOML, and a
`package.json.tmpl` is not JSON. Append `?format=` to the load path to say how the
file should be parsed, and the extension is no longer consulted:

```python
load("//foo:Cargo.lock?format=toml", lock = "value")
load("//foo:package.json.tmpl?format=json", tmpl = "value")
load("//foo:rules.bzl.in?format=bzl", "my_rule")
```

The accepted formats are `bzl`, `json`, and `toml`. `?format=bzl` evaluates the
file as Starlark, exactly as a `.bzl` file would be, so it exports whatever
symbols it defines rather than a single `value`.

A file loaded this way is a dependency of the build file that loads it, the same
as any `.bzl`: editing it invalidates the build file and everything downstream.

That also means an unparseable file is a **load-time** error, and a load-time
error fails every target in the package - not just the ones that use the data.
A malformed `Cargo.lock` will make `buck2 build //foo:some-unrelated-target` fail
too. This is the cost of the data being a tracked, cached dependency; if you need
a corrupt file to fail only the targets that read it, read it in an action
instead.

If a name other than `value` is needed, `load()` allows aliasing:

```python
load("//foo:bar.toml", my_more_specific_name = "value")

print(my_more_specific_name)
```

TOML types map unambiguously to JSON, and JSON types map unambiguously to
Starlark like you might expect:

- `null` maps to `None`, and `true`/`false` maps to `True`/`False`
- numbers map to `int` or `float`
- strings map to `str`
- arrays map to `list`
- objects map to `dict`
- tables (in TOML) map to `str`-keyed `dict`
