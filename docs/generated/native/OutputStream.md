<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# OutputStream



### Members

| Member | Type | Description |
|--------|------|-------------|
| ensure | `("") -> "ensured_artifact"` | Marks the artifact as an artifact that should be available to the users at the end of the bxl invocation. Any artifacts that do not get registered via this call is not accessible by users at the end of bxl script. |
| ensure_multiple | `("") -> ["ensured_artifact"]` | Same as `ensure`, but for multiple. |
| print | `(*[""], str.type) -> None` | Outputs results to the console via stdout. These outputs are considered to be the results of a bxl script, which will be displayed to stdout by buck2 even when the script is cached. Accepts an optional separator that defaults to " ". |
| print_json | `("") -> None` | Outputs results to the console via stdout as a json. These outputs are considered to be the results of a bxl script, which will be displayed to stdout by buck2 even when the script is cached. |


## ensure

```python
def ensure(artifact: "") -> "ensured_artifact"
```

Marks the artifact as an artifact that should be available to the users at the end of the bxl invocation. Any artifacts that do not get registered via this call is not accessible by users at the end of bxl script.

### Details

This function returns an `ensured_artifact` type that can be printed via `ctx.output.print()`
to print its actual path on disk.

---
## ensure_multiple

```python
def ensure_multiple(artifacts: "") -> ["ensured_artifact"]
```

Same as `ensure`, but for multiple.

---
## print

```python
def print(*args: [""], sep: str.type = None) -> None
```

Outputs results to the console via stdout. These outputs are considered to be the results of a bxl script, which will be displayed to stdout by buck2 even when the script is cached. Accepts an optional separator that defaults to " ".

### Details

Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
and `pprint`.

---
## print_json

```python
def print_json(value: "") -> None
```

Outputs results to the console via stdout as a json. These outputs are considered to be the results of a bxl script, which will be displayed to stdout by buck2 even when the script is cached.

### Details

Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
and `pprint`.
