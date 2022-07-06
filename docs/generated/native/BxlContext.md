<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# BxlContext



### Members

| Member | Type | Description |
|--------|------|-------------|
| analysis | `("", "", bool.type) -> ""` | Runs analysis on the given `labels`, accepting an optional `target_platform` which is the target platform configuration used to resolve configurations of any unconfigured target nodes, and an optional `skip_incompatible` boolean that indicates whether to skip analysis of nodes that are incompatible with the target platform. The `target_platform` is either a string that can be parsed as a target label, or a target label. |
| build | `("", "") -> ""` | Runs a build on the given `labels`, accepting an optional `target_platform` which is the target platform configuration used to resolve configurations. |
| bxl_actions | `"bxl_actions"` | Returns the action context [`BxlActionsCtx`] for creating and running actions. |
| cli_args | `""` | A struct of the command line args as declared using the [`cli_args`] module. These command lines are resolved per the users input on the cli when invoking the bxl script. |
| cquery | `("") -> "cqueryctx"` | Returns the [`StarlarkCQueryCtx`] that holds all the cquery functions. This function takes an optional parameter `target_platform`, which is the target platform configuration used to configured any unconfigured target nodes. |
| output | `""` | Gets the output stream to the console via stdout. Items written to the output stream are considered to be the results of a bxl script, which will be displayed to stdout by buck2 even when the script is cached. |
| root | `() -> str.type` | Returns the absolute path to the root of the repository |
| unstable_fs | `"fs"` | Returns the [`BxlFilesystem`] for performing a basic set of filesystem operations within bxl |
| uquery | `() -> "uqueryctx"` | Returns the [`StarlarkUQueryCtx`] that holds all uquery functions. |


## analysis

```python
def analysis(labels: "", target_platform: "" = None, skip_incompatible: bool.type = None) -> ""
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
def build(spec: "", target_platform: "" = None) -> ""
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
## bxl_actions : `"bxl_actions"`

Returns the action context [`BxlActionsCtx`] for creating and running actions.

---
## cli_args : `""`

A struct of the command line args as declared using the [`cli_args`] module. These command lines are resolved per the users input on the cli when invoking the bxl script.

---
## cquery

```python
def cquery(target_platform: "" = None) -> "cqueryctx"
```

Returns the [`StarlarkCQueryCtx`] that holds all the cquery functions. This function takes an optional parameter `target_platform`, which is the target platform configuration used to configured any unconfigured target nodes.

### Details

The `target_platform` is a target label, or a string that is a target label.

---
## output : `""`

Gets the output stream to the console via stdout. Items written to the output stream are considered to be the results of a bxl script, which will be displayed to stdout by buck2 even when the script is cached.

Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
and `pprint`.

---
## root

```python
def root() -> str.type
```

Returns the absolute path to the root of the repository

---
## unstable_fs : `"fs"`

Returns the [`BxlFilesystem`] for performing a basic set of filesystem operations within bxl

---
## uquery

```python
def uquery() -> "uqueryctx"
```

Returns the [`StarlarkUQueryCtx`] that holds all uquery functions.
