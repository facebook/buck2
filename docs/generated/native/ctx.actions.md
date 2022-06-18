<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# ctx.actions



### Members

| Member | Type | Description |
|--------|------|-------------|
| artifact_tag | `() -> Value < 'v >` | Allocate a new input tag |
| copied_dir | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| copy | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| copy_file | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| declare_output | `(& str, Option < & str >) -> StarlarkDeclaredArtifact` |  |
| download_file | `(Value < 'v >, & str, *, NoneOr < & str >, NoneOr < & str >, bool, bool) -> Value < 'v >` |  |
| download_file_new | `(Value < 'v >, & str, *, NoneOr < & str >, NoneOr < & str >, bool, bool) -> Value < 'v >` |  |
| dynamic_output | `(Vec < StarlarkArtifact >, Vec < StarlarkArtifact >, Vec < StarlarkOutputArtifact >, Value < 'v >) -> NoneType` |  |
| run | `(Value < 'v >, *, String, NoneOr < String >, Option < ValueOf < 'v, SmallMap < & 'v str, Value < 'v > > > >, bool, bool, i32, Option < ValueOf < 'v, SmallMap < & 'v str, Value < 'v > > > >, Option < String >, Option < String >, bool) -> NoneType` |  |
| symlink | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| symlink_file | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| symlinked_dir | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| tset | `(Value < 'v >, Option < Value < 'v > >, Option < Value < 'v > >) -> Value < 'v >` |  |
| write | `(Value < 'v >, Value < 'v >, *, bool, bool) -> Value < 'v >` |  |
| write_json | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |


## artifact_tag

```python
def artifact_tag() -> Value < 'v >
```

Allocate a new input tag

---
## copied_dir

```python
def copied_dir(output: Value < 'v >, srcs: Value < 'v >) -> Value < 'v >
```

---
## copy

```python
def copy(src: Value < 'v >, dest: Value < 'v >) -> Value < 'v >
```

---
## copy_file

```python
def copy_file(dest: Value < 'v >, src: Value < 'v >) -> Value < 'v >
```

---
## declare_output

```python
def declare_output(prefix: & str, filename: Option < & str > = None) -> StarlarkDeclaredArtifact
```

---
## download_file

```python
def download_file(
    output: Value < 'v >,
    url: & str,
    *,
    sha1: NoneOr < & str > = None,
    sha256: NoneOr < & str > = None,
    is_executable: bool = None,
    is_deferrable: bool = None
) -> Value < 'v >
```

---
## download_file_new

```python
def download_file_new(
    output: Value < 'v >,
    url: & str,
    *,
    sha1: NoneOr < & str > = None,
    sha256: NoneOr < & str > = None,
    is_executable: bool = None,
    is_deferrable: bool = None
) -> Value < 'v >
```

---
## dynamic_output

```python
def dynamic_output(dynamic: Vec < StarlarkArtifact >, inputs: Vec < StarlarkArtifact >, outputs: Vec < StarlarkOutputArtifact >, lambda: Value < 'v >) -> NoneType
```

---
## run

```python
def run(
    arguments: Value < 'v >,
    *,
    category: String,
    identifier: NoneOr < String > = None,
    env: Option < ValueOf < 'v, SmallMap < & 'v str, Value < 'v > > > > = None,
    local_only: bool = None,
    always_print_stderr: bool = None,
    weight: i32 = None,
    dep_files: Option < ValueOf < 'v, SmallMap < & 'v str, Value < 'v > > > > = None,
    metadata_env_var: Option < String > = None,
    metadata_path: Option < String > = None,
    no_outputs_cleanup: bool = None
) -> NoneType
```

---
## symlink

```python
def symlink(src: Value < 'v >, dest: Value < 'v >) -> Value < 'v >
```

---
## symlink_file

```python
def symlink_file(dest: Value < 'v >, src: Value < 'v >) -> Value < 'v >
```

---
## symlinked_dir

```python
def symlinked_dir(output: Value < 'v >, srcs: Value < 'v >) -> Value < 'v >
```

---
## tset

```python
def tset(definition: Value < 'v >, value: Option < Value < 'v > > = None, children: Option < Value < 'v > > = None) -> Value < 'v >
```

---
## write

```python
def write(output: Value < 'v >, content: Value < 'v >, *, is_executable: bool = None, allow_args: bool = None) -> Value < 'v >
```

---
## write_json

```python
def write_json(output: Value < 'v >, content: Value < 'v >) -> Value < 'v >
```
