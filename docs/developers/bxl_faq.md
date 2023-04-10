---
id: bxl_faqs
title: FAQs
---

## When should I use BXL over Buck2 API/CLI?

There are many overlaps between BXL and Buck2 (for example, both can run cquery and both can build targets). It’s possible that one use case could be handled by both BXL and Buck2.

Following are some specific recommendations to help decide when to use BXL over regular Buck2:

* **Use/inspect resolved attributes that are not exposed/accessible to users via normal Buck2 operations.**
  * This includes introspecting the Starlark object of providers, analyzing the Starlark object of a rule’s attr before and after coercing and resolution, and introspecting intermediate query results.
* **Reduce/eliminate the need to make several Buck2 calls within your program, such as running several subprocesses to call `cquery` several times.**
  * With BXL, you can just call the BXL script once in a subprocess, potentially reducing the amount of code you need to write in your program.
* **Reduce/eliminate the need to manually parse Buck2 output format within your program, and any bugs that may come with manual parsing**.
  * Some languages are more verbose than others when it comes to string parsing.
  * BXL scripts are written in Starlark, which is basically a deterministic, immutable Python, and are able to directly introspect Starlark objects (such as rules and target nodes, and so on) and call methods on these objects instead of parsing them over Buck2’s output.

## When is my BXL script cached?

The entire BXL script is represented as a single node on the DICE graph (Buck2’s internal dependency graph). When the script’s input changes, the entire node is invalidated and needs to be recomputed. For example, if a BXL function calls uquery, then uses the result to do a cquery and then a build, if Buck2 detects that any of the recorded calls to uquery, cquery, and build changes, then the entire BXL script will be reran. The computations themselves (uquery, cquery, and build) will still be incrementally evaluated via DICE, so we are not rerunning _every_ computation entirely within the BXL.

When the BXL script creates artifacts and ensures them, those artifacts are cached separately in an action outside of the BXL execution. This means that the artifacts produced by BXL are cached separately from the BXL script itself, much like the computations within a BXL.

During 2023, there is a plan to add finer grain incrementality to make better use of DICE’s existing incrementality support.

## What’s the difference between `ctx.output.print()` and `print()`?

* `ctx.output.print()` writes items to stdout by buck2 even when the script is cached. Items written to the output stream are considered to be the results of a BXL script, which will be displayed to stdout by buck2 even when the script is cached.
* `print()` is offered by Starlark via the stdlib. This prints anything you want but won’t be provided to stdout at the end of a BXL script. These can be used to print to stderr. NOTE: `print()` statements don't show up if the script has been cached.

## What do I need to know about ensured artifacts

An `ensured_artifact` prints out the relative or absolute path via `ctx.output.print()`, depending on if called with `abs_path()` or `rel_path`(), but will print out `<ensured artifact bound to <some path>>` via `print()`.

This is intentional because when the ensured artifact is created with BXL within BXL, it has not been materialized yet. It will be materialized after the BXL script finishes executing, and Buck2 core performs some additional actions after the BXL script.

This is a safeguard to prevent people from misusing the artifact path and passing it into an action without the artifact having been materialized or passing an absolute path into RE, which can actually mess up RE and render the action not shareable across users. In addition, it makes these actions separately cacheable from the BXL execution.
