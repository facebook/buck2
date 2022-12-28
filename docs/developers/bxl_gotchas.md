---
id: bxl_gotchas
title: Things to Keep in Mind
---

This page contains a non-exhaustive list of things to keep in mind when writing a BXL script.

## Avoid getting the artifact path before materialization (in most cases)

BXL has a `get_path_without_materialization()` function on artifacts. This is a risky function to call because you may accidentally pass this path to further BXL actions that expect the artifact to be materialized. If this happens, the BXL script will error out.

If you want the path without materialization for other uses that don’t involve passing them into further actions, then it’s safe.

It is slower to loop through objects and ensure them one by one than it is to call `ensure_multiple()` on all the objects at once (if possible). This is due to the fact that Starlark is an interpreted language, which is already slower than BXL core (in Rust),  In addition, going between Starlark and Rust between the multiple `ensure()` calls also takes time.

## Checking if something is in a dict

Using `if x in my_dict` is faster than using `if x in my_dict.items()`. This is attributed to the overhead when going between Starlark and Rust for the `.items()` call on the dict to retrieve the Starlark objects from the heap, whereas using `if x in my_dict`, there doesn’t need to be an additional trip to Rust.

## You cannot get a non-configured label from a configured target node

You cannot get a non-configured label from a configured target node because the configured target node is not uniquely identified a non-configured label, only by the configured target label.

## Using a single `eval()` is faster than using nested queries

Following is a sample of a nested query, not using `eval()`:

```buck2
ctx.cquery().kind("(apple|cxx)_library", ctx.cquery().deps(targets))
```

For complex/large queries, using a single `eval()` is probably going to be marginally faster than breaking it up into individual queries and calling them one at a time:

```buck2
ctx.cquery().eval(“kind((apple|cxx)_library, deps(targets))”)
```

However, breaking up a large, nested query into individual BXL query calls can aid in readability/writeability.
