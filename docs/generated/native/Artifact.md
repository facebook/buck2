<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# Artifact

A single input or output for an action

### Members

| Member | Type | Description |
|--------|------|-------------|
| as_output | `() -> StarlarkOutputArtifact` | Returns a `StarlarkOutputArtifact` instance, or fails if the artifact is either an `Artifact`, or is a bound `Artifact` (You cannot bind twice) |
| basename | `StringValue < 'v >` | The base name of this artifact. e.g. for an artifact at `foo/bar`, this is `bar` |
| extension | `StringValue < 'v >` | The file extension of this artifact. e.g. for an artifact at foo/bar.sh, this is `.sh`. If no extension is present, `""` is returned. |
| is_source | `bool` | Whether the artifact represents a source file |
| owner | `Option < Label < 'v > >` | The `Label` of the rule that originally created this artifact. May also be None in the case of source files, or if the artifact has not be used in an action, or if the |
| project | `(& str) -> StarlarkArtifact` |  |
| short_path | `StringValue < 'v >` | The interesting part of the path, relative to somewhere in the output directory. For an artifact declared as `foo/bar`, this is `foo/bar`. |


## as_output

```python
def as_output() -> StarlarkOutputArtifact
```

Returns a `StarlarkOutputArtifact` instance, or fails if the artifact is either an `Artifact`, or is a bound `Artifact` (You cannot bind twice)

---
## basename : `StringValue < 'v >`

The base name of this artifact. e.g. for an artifact at `foo/bar`, this is `bar`

---
## extension : `StringValue < 'v >`

The file extension of this artifact. e.g. for an artifact at foo/bar.sh, this is `.sh`. If no extension is present, `""` is returned.

---
## is_source : `bool`

Whether the artifact represents a source file

---
## owner : `Option < Label < 'v > >`

The `Label` of the rule that originally created this artifact. May also be None in the case of source files, or if the artifact has not be used in an action, or if the

---
## project

```python
def project(path: & str) -> StarlarkArtifact
```

---
## short_path : `StringValue < 'v >`

The interesting part of the path, relative to somewhere in the output directory. For an artifact declared as `foo/bar`, this is `foo/bar`.
