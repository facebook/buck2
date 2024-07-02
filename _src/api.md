# APIs

A lot of Buck2 is driven by Starlark APIs. While there is a
[Starlark specification](https://github.com/bazelbuild/starlark/blob/master/spec.md),
for most purposes it can be considered a subset of Python. There are three main
places you can write Starlark in Buck2:

- In `BUCK` files, where you can define the rules. The most interesting
  functions are [the rules themselves](rules), but you will often use the
  [builtin Starlark functions](starlark/globals) (most of which are the same as
  in Python), and a few of the [build functions](build/globals) (e.g. `glob`).
- In rule definitions, where you can use the same Starlark standard functions,
  but will heavily be using the [build functions](build/globals) (e.g. `rule`
  and `attrs`).
- In [BXL](../developers/bxl), where the [context type](bxl/bxl_ctx) is one of
  the more important ones.
