<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# ctx.actions



### Members

| Member | Type | Description |
|--------|------|-------------|
| artifact_tag | `() -> ""` | Allocate a new input tag |
| copied_dir | `("", "") -> ""` |  |
| copy_file | `("", "") -> ""` |  |
| declare_output | `(str.type, [None, str.type]) -> "artifact"` |  |
| download_file | `("", str.type, *, [None, str.type], [None, str.type], bool.type, bool.type) -> ""` |  |
| dynamic_output | `(["artifact"], ["artifact"], ["output_artifact"], "") -> None` |  |
| run | `("", *, str.type, [None, str.type], [None, {str.type: ""}], bool.type, bool.type, bool.type, int.type, [None, {str.type: ""}], [None, str.type], [None, str.type], bool.type) -> None` |  |
| symlink_file | `("", "") -> ""` |  |
| symlinked_dir | `("", "") -> ""` |  |
| tset | `("", [None, ""], [None, ""]) -> ""` |  |
| write | `("", "", *, bool.type, bool.type) -> ""` |  |
| write_json | `("", "") -> ""` |  |


## artifact_tag

```python
def artifact_tag() -> ""
```

Allocate a new input tag

---
## copied_dir

```python
def copied_dir(output: "", srcs: "") -> ""
```

---
## copy_file

```python
def copy_file(dest: "", src: "") -> ""
```

---
## declare_output

```python
def declare_output(prefix: str.type, filename: [None, str.type] = None) -> "artifact"
```

---
## download_file

```python
def download_file(
    output: "",
    url: str.type,
    *,
    sha1: [None, str.type] = None,
    sha256: [None, str.type] = None,
    is_executable: bool.type = None,
    is_deferrable: bool.type = None
) -> ""
```

---
## dynamic_output

```python
def dynamic_output(dynamic: ["artifact"], inputs: ["artifact"], outputs: ["output_artifact"], lambda: "") -> None
```

---
## run

```python
def run(
    arguments: "",
    *,
    category: str.type,
    identifier: [None, str.type] = None,
    env: [None, {str.type: ""}] = None,
    local_only: bool.type = None,
    prefer_local: bool.type = None,
    always_print_stderr: bool.type = None,
    weight: int.type = None,
    dep_files: [None, {str.type: ""}] = None,
    metadata_env_var: [None, str.type] = None,
    metadata_path: [None, str.type] = None,
    no_outputs_cleanup: bool.type = None
) -> None
```

---
## symlink_file

```python
def symlink_file(dest: "", src: "") -> ""
```

---
## symlinked_dir

```python
def symlinked_dir(output: "", srcs: "") -> ""
```

---
## tset

```python
def tset(definition: "", value: [None, ""] = None, children: [None, ""] = None) -> ""
```

---
## write

```python
def write(output: "", content: "", *, is_executable: bool.type = None, allow_args: bool.type = None) -> ""
```

---
## write_json

```python
def write_json(output: "", content: "") -> ""
```
