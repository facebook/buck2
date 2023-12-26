---
id: bxl_basics
title: BXL Basics
---

This page is a primer on common BXL functionalities and data types. Ramping up
in BXL may be challenging without much prior knowledge of Buck2 building blocks
(ex: targets, configurations, queries), so please take a look at the
[Concepts](../concepts/concept_map.md) documentation before reading on.

## Common BXL functionalities

### Build

You can build targets within BXL with
[`ctx.build()`](../../api/bxl/bxl_ctx/#bxl_ctxbuild). The result is a
[`bxl_build_result`](../../api/bxl/bxl_build_result), which has `artifacts()`
and `failures()` functions that provide iterators to the artifacts or failures,
respectively. You can pass in a single target or target pattern to build.

### Actions

You can create actions directly within the BXL API. The available action APIs
are equivalent to the ones found on the [`actions`](../../api/bxl/actions) type
for normal rules, with the caveat that
[dynamic actions](./bxl_dynamic_output.md) use the
[`bxl_ctx`](../../api/bxl/bxl_ctx) (which provides richer functionalities).

A common workflow would be to run analysis on a target, and use some interesting
bits found in the analysis result to construct an augmented
[`cmd_args`](../../api/bxl/cmd_args) to run, and then ensure the action's output
(see below for ensuring). Also see
[Running actions](./bxl_common_how_tos.md#running-actions).

### Ensure

Ensuring an artifact means that you want the artifact to be materialized
(meaning, downloaded to your machine) at the end of the BXL execution. There are
two APIs for ensuring: `ctx.output.ensure()` and `ctx.output.ensure_multiple()`
(see [`bxl_output_stream`](../../api/bxl/bxl_output_stream)). As the naming
indicates, the former is for ensuring a single artifact, and the latter is for
ensuring multiple artifact-like inputs. Artifact-like inputs include
[`cmd_args`](../../api/bxl/cmd_args) (can be found when inspecting providers),
[`bxl_build_result`](../../api/bxl/bxl_build_result) (produced when building
something in BXL), or [`artifact`](../../api/bxl/artifact) (can be found when
inspecting providers, or creating your own actions).

A common workflow is to ensure an artifact that you created via some custom
actions defined in your script, or ensuring some artifacts found in the
providers after running analysis. Also see
[What do I need to know about ensured artifacts](./bxl_faq.md#what-do-i-need-to-know-about-ensured-artifacts).
