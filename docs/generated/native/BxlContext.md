<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# BxlContext



### Members

| Member | Type | Description |
|--------|------|-------------|
| analysis | `(Value < 'v >, Value < 'v >, bool) -> Value < 'v >` | Runs analysis on the given `labels`, accepting an optional `target_platform` which is the target platform configuration used to resolve configurations of any unconfigured target nodes, and an optional `skip_incompatible` boolean that indicates whether to skip analysis of nodes that are incompatible with the target platform. The `target_platform` is either a string that can be parsed as a target label, or a target label. |
| build | `(Value < 'v >, Value < 'v >) -> Value < 'v >` | Runs a build on the given `labels`, accepting an optional `target_platform` which is the target platform configuration used to resolve configurations. |
| bxl_actions | `BxlActionsCtx < 'v >` | Returns the action context [`BxlActionsCtx`] for creating and running actions. |
| cli_args | `Value < 'v >` | A struct of the command line args as declared using the [`cli_args`] module. These command lines are resolved per the users input on the cli when invoking the bxl script. |
| cquery | `(Value < 'v >) -> StarlarkCQueryCtx < 'v >` | Returns the [`StarlarkCQueryCtx`] that holds all the cquery functions. This function takes an optional parameter `target_platform`, which is the target platform configuration used to configured any unconfigured target nodes. |
| output | `Value < 'v >` | Gets the output stream to the console via stdout. Items written to the output stream are considered to be the results of a bxl script, which will be displayed to stdout by buck2 even when the script is cached. |
| root | `() -> String` | Returns the absolute path to the root of the repository |
| unstable_fs | `BxlFilesystem < 'v >` | Returns the [`BxlFilesystem`] for performing a basic set of filesystem operations within bxl |
| uquery | `() -> StarlarkUQueryCtx < 'v >` | Returns the [`StarlarkUQueryCtx`] that holds all uquery functions. |


## analysis

```python
def analysis(labels: Value < 'v >, target_platform: Value < 'v > = None, skip_incompatible: bool = None) -> Value < 'v >
```

Runs analysis on the given `labels`, accepting an optional `target_platform` which is the target platform configuration used to resolve configurations of any unconfigured target nodes, and an optional `skip_incompatible` boolean that indicates whether to skip analysis of nodes that are incompatible with the target platform. The `target_platform` is either a string that can be parsed as a target label, or a target label.

### Details

The given `labels` is a providers expression, which is either:
    - a single string that is a `target pattern`.
    - a single target node or label, configured or unconfigured
    - a single sub target label, configured or unconfigured
    - a list of the two options above.

This returns either a single [`StarlarkAnalysisResult`] if the given `labels` is "singular",
or a dict keyed by sub target labels of [`StarlarkAnalysisResult`] if the given `labels`
is list-like

---
## build

```python
def build(spec: Value < 'v >, target_platform: Value < 'v > = None) -> Value < 'v >
```

Runs a build on the given `labels`, accepting an optional `target_platform` which is the target platform configuration used to resolve configurations.

### Details

The given `labels` is a providers expression, which is either:
    - a single string that is a `target pattern`.
    - a single target node or label, configured or unconfigured
    - a single provider label, configured or unconfigured
    - a list of the two options above.

This returns a dict keyed by sub target labels of [`StarlarkBuildResult`] if the
given `labels` is list-like

---
## bxl_actions : `BxlActionsCtx < 'v >`

Returns the action context [`BxlActionsCtx`] for creating and running actions.

---
## cli_args : `Value < 'v >`

A struct of the command line args as declared using the [`cli_args`] module. These command lines are resolved per the users input on the cli when invoking the bxl script.

---
## cquery

```python
def cquery(target_platform: Value < 'v > = None) -> StarlarkCQueryCtx < 'v >
```

Returns the [`StarlarkCQueryCtx`] that holds all the cquery functions. This function takes an optional parameter `target_platform`, which is the target platform configuration used to configured any unconfigured target nodes.

### Details

The `target_platform` is a target label, or a string that is a target label.

---
## output : `Value < 'v >`

Gets the output stream to the console via stdout. Items written to the output stream are considered to be the results of a bxl script, which will be displayed to stdout by buck2 even when the script is cached.

Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
and `pprint`.

---
## root

```python
def root() -> String
```

Returns the absolute path to the root of the repository

---
## unstable_fs : `BxlFilesystem < 'v >`

Returns the [`BxlFilesystem`] for performing a basic set of filesystem operations within bxl

---
## uquery

```python
def uquery() -> StarlarkUQueryCtx < 'v >
```

Returns the [`StarlarkUQueryCtx`] that holds all uquery functions.
