<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# StarlarkAnalysisResult



### Members

| Member | Type | Description |
|--------|------|-------------|
| providers | `() -> ""` | Access the providers of the rule. Returns a 'ProviderCollection' the same as accessing providers of dependencies within a rule implementation. |


## providers

```python
def providers() -> ""
```

Access the providers of the rule. Returns a 'ProviderCollection' the same as accessing providers of dependencies within a rule implementation.
