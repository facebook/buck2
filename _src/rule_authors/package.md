---
id: package_files
title: PACKAGE Files
---

`PACKAGE` files are per-directory configuration files which are accessible from
Starlark rules/macros. It supports things like per-directory properties, reading
parent `PACKAGE` values (`read_parent_package_value()`), writing `PACKAGE`
values (`write_package_value()`), loading helper `bzl` files, and you can also
inspect `PACKAGE` values via `buck2 audit package-values`.

Before evaluating `BUCK` file, buck2 will evaluate all `PACKAGE` files in the
same directory and all parent directories. Absent `PACKAGE` files are treated as
empty files.

All relevant `PACKAGE` files are executed sequentially from the root directory
to the current directory (but unrelated `PACKAGE` files can be executed in
parallel). Evaluating `PACKAGE` files sequentially provides additional
guarantees, for example, attempt to override a property (unless explicitly
requested) should fail with Starlark call stack.

Each `PACKAGE` file is evaluated at most once (like `bzl` files).

`PACKAGE` files may load arbitrary `bzl` files. `BUCK`-specific functions called
in `bzl` files (like rule functions) are available, but calling functions from
`PACKAGE` files is an error. This way, `bzl` files are evaluated only once
regardless of whether they are loaded from `PACKAGE` or `BUCK` file.

## APIs

### `PACKAGE` APIs

#### [`write_package_value`](../../api/build#write_package_value)

```python
def write_package_value(
    name: str,
    value: "",
    overwrite: bool = False,
): ...
```

This global API is only available in `PACKAGE` files, or `bzl` files included in
`PACKAGE` files.

`name` is a string which must contain exactly one dot symbol (just to enforce
code style).

`value` is an arbitrary Starlark value, for example, an integer, a list of
integer, a struct or a function. The value must be serializable into JSON.

When `overwrite` is `False` (default), attempt to overwrite per-`PACKAGE` value
defined in parent `PACKAGE` file will fail.

Written values are frozen when `PACKAGE` file evaluation is finished.

Note `write_package_value` symbol exists in `bzl` globals, and it can be called
from `bzl` file in context of `PACKAGE` evaluation, but calling
`write_package_file` is an error on context of `BUCK` evaluation.

Modifying `PACKAGE` file logically invalidates the `BUCK` file of this
directory, and all `PACKAGE` and `BUCK` files of sub-`PACKAGE`s. However, `BUCK`
file evaluation may track which `PACKAGE`-local values were accessed and only
invalidate `BUCK` files which were potentially affected (similarly to how we do
it with buckconfigs).

#### [`read_parent_package_value`](../../api/build#read_parent_package_value)

```python
def read_parent_package_value(
    key: str,
): ...
```

This global API is only available in `PACKAGE` files, or `bzl` files included in
`PACKAGE` files.

This function returns the `PACKAGE` value defined in a parent `PACKAGE` file, or
`None` is such value does not exist.

This function is available in `PACKAGE` files, but attempt to call this function
in context of `bzl` file evaluation results in an error.

#### [`package`](../../api/build#package)

```python
def package(
    inherit: bool = False,
    visibility: list[str] | tuple[str, ...] = [],
    within_view: list[str] | tuple[str, ...] = []
) -> None
```

This global API is only available in `PACKAGE` files, or `bzl` files included in
`PACKAGE` files.

`visibility` is a list of visibility patterns to apply to all targets contained
within the directory, unless the target defines it's own visibility patterns.

`within_view` is a list of visibility patterns restricting what all target
contained within the `PACKAGE` directory can depend on. Applies to first-order
deps, and not transitive deps.

If `inherit` is `True`, then the `visibility` and `within_view` will be
inherited from the nearest parent `PACKAGE`.

#### [`read_config`](../../api/build#read_config)

`PACKAGE` files are able to call `read_config` to read buckconfigs.

### `BUCK`-specific API

#### [`read_package_value`](../../api/build#read_package_value)

```python
def read_package_value(
    name: str,
): ...
```

This global API is only available in `BUCK` files, or `bzl` files included in
`BUCK` files.

This function returns the nearest `name` value registered per `PACKAGE`, or
`None` is such value does not exist.

This function is available in `bzl` files, but attempt to call this function in
context of `PACKAGE` file evaluation results in an error. This restriction can
be lifted in the future.
