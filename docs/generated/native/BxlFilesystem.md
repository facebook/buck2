<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# BxlFilesystem



### Members

| Member | Type | Description |
|--------|------|-------------|
| exists | `(str.type) -> bool.type` | check if a path exists on disk, taking advantage of Buck's cached filesystem |


## exists

```python
def exists(path: str.type) -> bool.type
```

check if a path exists on disk, taking advantage of Buck's cached filesystem
