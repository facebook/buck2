<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# bxl_actions



### Members

| Member | Type | Description |
|--------|------|-------------|
| action_factory | `() -> Value < 'v >` | Returns the actions context [`AnalysisRegistry`] to create and register actions for this bxl function. This will have the same functionality as the actions for rules. |


## action_factory

```python
def action_factory() -> Value < 'v >
```

Returns the actions context [`AnalysisRegistry`] to create and register actions for this bxl function. This will have the same functionality as the actions for rules.

### Details

Actions created by bxl will not be built by default. Instead, they are marked to be built
by `ctx.output.ensure(artifact)` on the output module of the [`BxlContext`]. Only artifacts
marked by ensure will be built.
