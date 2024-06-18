---
id: buck2_library
title: Consuming code from the `buck2` repository
---

It is possible to depend on targets from the Buck2 repository, which you
might need to do to write a custom test runner (at least until [TestInfo
V2](../../rfcs/drafts/test-info-v2.md) lands). Note that there are no
API stability guarantees at the moment.

The Buck2 Rust code is available in the `buck2` cell and is useable as
is after adding the following to your `.buckconfig`'s (assuming that the
`buck2` project is available at `path/to/buck2`):

```
[cells]
buck2 = path/to/buck2
shim = path/to/buck2/shim

[cell_aliases]
config = prelude
ovr_config = prelude
fbcode = shim
fbsource = shim
fbcode_macros = shim
buck = shim
bazel_skylib = shim
```

Note that due to how cells work in Buck2, the `buck2` cell cannot bring
its own dependencies, so you must remove conflicting cell aliases from
`path/to/buck2/.buckconfig`.
