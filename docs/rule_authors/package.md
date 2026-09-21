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

#### [`enforce_visibility_intersection`](../../api/build#enforce_visibility_intersection)

```python
def enforce_visibility_intersection() -> None
```

This global API is only available in `PACKAGE` files. Unlike the other `PACKAGE`
APIs, calling it from a `bzl` file included in a `PACKAGE` file results in an
error. It may be called at most once per `PACKAGE` file.

By default, `package(visibility=...)` only supplies a default visibility: a
target that declares its own `visibility` ignores the `PACKAGE` visibility
entirely. `enforce_visibility_intersection()` changes this to
intersection-based visibility for the current `PACKAGE` and all of its
descendants: every target's effective visibility is the intersection (logical
AND) of its own `visibility` and a propagating cap.

The cap is built from the explicit `package(visibility=...)` list of each
opted-in ancestor `PACKAGE`. Because the cap only tightens visibility, a target
can never be made visible to more than its own `visibility` allows. Declaring a
broader `visibility` on the target cannot escape the cap.

`"PUBLIC"` is the identity of the intersection, so a target with
`visibility=["PUBLIC"]` is silently clipped to the cap rather than rejected.

Calling `enforce_visibility_intersection()` without a non-`None`
`package(visibility=...)` in the same file (i.e. `visibility` omitted or set to
`None`) contributes nothing to the cap. The parent's cap simply propagates
unchanged, so a directory can opt into enforcement without further narrowing
what its parents already allow.

The propagated cap can be inspected via `buck2 audit package-values`. When a
visibility check fails because of the cap, the error reports the cap that
blocked it.

#### [`enforce_within_view_intersection`](../../api/build#enforce_within_view_intersection)

```python
def enforce_within_view_intersection() -> None
```

This global API is only available in `PACKAGE` files. Like
`enforce_visibility_intersection()`, calling it from a `bzl` file included in a
`PACKAGE` file results in an error, and it may be called at most once per
`PACKAGE` file.

By default, `package(within_view=...)` only supplies a default `within_view`: a
target that declares its own `within_view` ignores the `PACKAGE` list entirely,
and a nested `package()` call (with `inherit=False`, the default) replaces the
inherited list for its subtree. `enforce_within_view_intersection()` changes
this to intersection-based `within_view` for the current `PACKAGE` and all of
its descendants: every target's effective `within_view` is the intersection
(logical AND) of its own `within_view` (declared, or defaulted from `package()`)
and a propagating cap.

The cap is built from the `package(within_view=...)` list of each opted-in
ancestor `PACKAGE`. Because the cap only tightens `within_view`, a directory
tree can make "what my targets may depend on" a property that no descendant can
widen: neither a target declaring a broader `within_view` (including
`within_view=["PUBLIC"]`) nor a nested `package(within_view=["PUBLIC"])` can
escape the cap. Dependencies within the same package, and dependencies that come
from an attribute's default value, are exempt, as they are for `within_view`
generally.

`"PUBLIC"` is the identity of the intersection, so calling
`enforce_within_view_intersection()` in a `PACKAGE` whose `package()` call omits
`within_view` (or sets it to `["PUBLIC"]`) contributes nothing to the cap. The
parent's cap simply propagates unchanged. Note that `within_view=[]` also parses
to `PUBLIC` and so contributes nothing, unlike `visibility=[]`, which is an
empty visibility cap. `inherit=True` still unions the `within_view` *default*
with the parent's as usual; the cap is independent of `inherit` and always
intersects, so an `inherit=True` child whose own list is disjoint from the
parent's cap yields a cap that no cross-package dependency satisfies.

The propagated cap can be inspected via `buck2 audit package-values`. Inside an
opted-in subtree, a refused dependency is reported together with both the
target's own `within_view` and the cap, since widening the target's own list
alone cannot get past the cap.

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
